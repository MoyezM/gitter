open! Core
open Async

(* The queries the UI actually asks: run git, parse with the pure parsers. *)

let status () =
  (* -uall: enumerate files INSIDE untracked directories — the default
     collapses a fully-untracked dir into one "dir/" entry, which can't be
     diffed and hides its contents from review. The raw output rides along
     as the poller's cheap change signature. *)
  let%map.Deferred.Or_error output =
    Runner.git [ "status"; "--porcelain=v2"; "--branch"; "--untracked-files=all" ]
  in
  output, Status.parse output, Status.branch output
;;

(* The file's content at HEAD — the base of a staged diff. *)
let file_at_rev ~rev path = Runner.git [ "show"; rev ^ ":" ^ path ]
let file_at_head path = file_at_rev ~rev:"HEAD" path

(* The file's content in the INDEX (stage 0) — the base of an unstaged
   diff, and the result side of a staged one. *)
let file_in_index path = Runner.git [ "show"; ":0:" ^ path ]

(* The file's combined (staged + unstaged) change vs HEAD — what "what did I
   change" means in the uncommitted view. Untracked files have no diff vs
   HEAD, so fall back to a --no-index diff against /dev/null (which exits 1
   when the file is non-empty, hence [accept_nonzero_exit]). *)
(* Parsing runs in a domain: a 530k-line diff (whole-file-added 17MB) costs
   ~110ms, which would stall ~7 frames on the scheduler. *)
let parse_off_thread output =
  let%map.Deferred files = Cpu.in_domain (fun () -> Diff.parse output) in
  Ok files
;;

(* The UNSTAGED change: worktree vs index. Untracked files have no index
   entry (plain diff shows nothing for them), so fall back to a /dev/null
   whole-file-added diff — ls-files distinguishes them from files that are
   merely unchanged. *)
let diff_unstaged path =
  match%bind.Deferred.Or_error Runner.git [ "diff"; "--no-color"; "--"; Runner.literal path ] with
  | "" ->
    (match%bind.Deferred Sys.is_directory path with
     | `Yes -> Deferred.Or_error.return []
     | `No | `Unknown ->
       (match%bind.Deferred
          Runner.git [ "ls-files"; "--error-unmatch"; "--"; Runner.literal path ]
        with
        | Ok _ -> Deferred.Or_error.return [] (* tracked, genuinely unchanged *)
        | Error _ ->
          let%bind.Deferred.Or_error output =
            Runner.git
              ~accept_nonzero_exit:[ 1 ]
              [ "diff"; "--no-color"; "--no-index"; "--"; "/dev/null"; path ]
          in
          parse_off_thread output))
  | output -> parse_off_thread output
;;

(* The STAGED change: index vs HEAD. *)
let diff_staged path =
  let%bind.Deferred.Or_error output =
    Runner.git [ "diff"; "--no-color"; "--cached"; "--"; Runner.literal path ]
  in
  parse_off_thread output
;;

(* ---- the branch stack (pure git; see Stack) ---------------------------- *)

(* Cheap change signature for the poller: any local ref move shows up. *)
let refs_signature ~extra () =
  match%map.Deferred
    Runner.git
      (* [extra]: the review target's origin refs, when one is active — a
         fetch moving them must refresh the view, but watching all of
         refs/remotes would make every unrelated fetch a full refresh *)
      ([ "for-each-ref"; "refs/heads" ]
       @ extra
       @ [ "--format=%(refname:short)%09%(objectname)" ])
  with
  | Ok output -> output
  | Error e -> "error:" ^ Error.to_string_hum e
;;

(* Bounded so a branch far behind trunk can't make the DAG walk huge; the
   parser degrades gracefully when truncated (distant branches attach to
   the trunk). *)
let dag_commit_cap = 5000
let reflog_tip_cap = 20
let reflog_branch_cap = 40

let stack () =
  let open Deferred.Or_error.Let_syntax in
  (* Recency-sorted so the reflog budget goes to the branches that are
     actually being worked on, not the alphabetical first 40. *)
  let%bind heads_raw =
    Runner.git
      [ "for-each-ref"
      ; "refs/heads"
      ; "--sort=-committerdate"
      ; "--format=%(refname:short)%09%(objectname)"
      ]
  in
  let heads =
    String.split_lines heads_raw |> List.filter_map ~f:(fun l -> String.lsplit2 l ~on:'\t')
  in
  match heads with
  | [] -> return []
  | _ ->
    let names = List.map heads ~f:fst in
    let%bind current =
      match%map.Deferred Runner.git [ "branch"; "--show-current" ] with
      | Error _ -> Ok None
      | Ok s ->
        let s = String.strip s in
        Ok (Option.some_if (not (String.is_empty s)) s)
    in
    (* Trunk: origin's default branch when it exists locally, else
       main/master, else wherever we are. *)
    let%bind origin_head =
      match%map.Deferred Runner.git [ "symbolic-ref"; "--short"; "refs/remotes/origin/HEAD" ] with
      | Error _ -> Ok None
      | Ok s ->
        let s = String.strip s in
        Ok (Some (Option.value_map (String.lsplit2 s ~on:'/') ~default:s ~f:snd))
    in
    let mem n = List.mem names n ~equal:String.equal in
    let trunk =
      List.find_map
        [ origin_head; Some "main"; Some "master"; current; List.hd names ]
        ~f:(fun c -> Option.bind c ~f:(fun c -> Option.some_if (mem c) c))
      |> Option.value ~default:(List.hd_exn names)
    in
    (* The DAG's floor: each branch's fork point off the trunk, excluded
       from the walk — leaving exactly the stack-unique commits (plus any
       trunk advance past the forks). Per-branch merge-bases rather than
       one --octopus: a single unrelated-history branch fails octopus for
       everyone, while here it just contributes no floor (its commits are
       then bounded by the cap alone). *)
    let%bind bases =
      match List.Assoc.find heads trunk ~equal:String.equal with
      | None -> return []
      | Some trunk_sha ->
        let others =
          List.filter_map heads ~f:(fun (n, sha) ->
            Option.some_if (not (String.equal n trunk)) sha)
          |> List.dedup_and_sort ~compare:String.compare
        in
        (match others with
         | [] -> return [ trunk_sha ]
         | _ ->
           Deferred.List.map others ~how:(`Max_concurrent_jobs 8) ~f:(fun sha ->
             match%map.Deferred Runner.git [ "merge-base"; trunk_sha; sha ] with
             | Ok out -> Some (String.strip out)
             | Error _ -> None (* unrelated history: no floor from this one *))
           |> Deferred.map ~f:(fun mbs ->
             Ok (List.filter_opt mbs |> List.dedup_and_sort ~compare:String.compare)))
    in
    let%bind dag =
      Runner.git
        ([ "rev-list"; "--parents"; sprintf "--max-count=%d" dag_commit_cap; "--branches" ]
         @ (if List.is_empty bases then [] else "--not" :: bases))
    in
    let%map reflogs =
      List.take names reflog_branch_cap
      |> Deferred.List.map ~how:(`Max_concurrent_jobs 8) ~f:(fun name ->
        match%map.Deferred
          Runner.git
            [ "log"; "-g"; sprintf "--max-count=%d" reflog_tip_cap; "--format=%H"
            ; "refs/heads/" ^ name
            ]
        with
        | Error _ -> "" (* no reflog for this ref — fine *)
        | Ok output ->
          String.split_lines output
          |> List.filter ~f:(Fn.non String.is_empty)
          |> List.map ~f:(fun sha -> name ^ "\t" ^ sha)
          |> String.concat ~sep:"\n")
      |> Deferred.map ~f:(fun xs -> Ok (String.concat xs ~sep:"\n"))
    in
    Branch_stack.parse ~heads:heads_raw ~dag ~reflogs ~trunk ~current
;;

(* Per-file +/- line counts per side (the panes also sum them for the
   title bars). Binary files (numstat prints "-") are skipped; untracked
   files are not in numstat, so their whole-file adds are not counted. *)
let diffstat () =
  let by_path output =
    Diff.numstat output |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind staged = Runner.git [ "diff"; "--no-color"; "--numstat"; "--cached" ] in
  let%map unstaged = Runner.git [ "diff"; "--no-color"; "--numstat" ] in
  by_path staged, by_path unstaged
;;

(* ---- the committed view (this branch vs its base) ---------------------- *)

(* What the branch adds over [base], merge-base semantics (the three-dot
   range): entries, per-file +/- counts, and each file's (old blob, new
   blob) pair — the review-mark key. A missing/unrelated base yields
   empty — the pane shows its idle message. *)
let committed ~base ~head () =
  let range = base ^ "..." ^ head in
  let open Deferred.Or_error.Let_syntax in
  (* independent reads of the same range — in parallel; review-mode
     ranges are branch-sized, so serializing them is a visible cost *)
  let raw_d = Runner.git [ "diff"; "--no-color"; "--raw"; "--no-abbrev"; "-M"; range ] in
  let stats_d = Runner.git [ "diff"; "--no-color"; "--numstat"; "-M"; range ] in
  let%bind raw = raw_d in
  let%map stats = stats_d in
  let parsed = Status.parse_raw raw in
  ( List.map parsed ~f:fst
  , Diff.numstat stats |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  , List.map parsed ~f:(fun (e, blobs) -> e.Status.Entry.path, blobs)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first) )
;;

let merge_base a b =
  Deferred.Or_error.map (Runner.git [ "merge-base"; a; b ]) ~f:String.strip
;;

let diff_committed ~base ~head path =
  let%bind.Deferred.Or_error output =
    Runner.git
      [ "diff"; "--no-color"; "-M"; base ^ "..." ^ head; "--"; Runner.literal path ]
  in
  parse_off_thread output
;;

(* Review-target resolution: the pushed version is the reviewable truth,
   so prefer origin/<name> when that ref exists (a local remote-tracking
   ref — no network is ever touched). *)
let resolve_review_ref ~prefer_origin name =
  if not prefer_origin
  then Deferred.return name
  else (
    let remote = "origin/" ^ name in
    match%map.Deferred
      Runner.git [ "rev-parse"; "--verify"; "--quiet"; remote ^ "^{commit}" ]
    with
    | Ok (_ : string) -> remote
    | Error (_ : Error.t) -> name)
;;
