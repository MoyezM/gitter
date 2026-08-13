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
let file_at_head path = Runner.git [ "show"; "HEAD:" ^ path ]

(* The file's content in the INDEX (stage 0) — the base of an unstaged
   diff, and the result side of a staged one. *)
let file_in_index path = Runner.git [ "show"; ":0:" ^ path ]

(* The file's combined (staged + unstaged) change vs HEAD — what "what did I
   change" means in the uncommitted view. Untracked files have no diff vs
   HEAD, so fall back to a --no-index diff against /dev/null (which exits 1
   when the file is non-empty, hence [accept_nonzero_exit]). *)
(* Parsing runs in a domain: a 530k-line diff (whole-file-added 17MB) costs
   ~110ms, which would stall ~7 frames on the scheduler. On exhaustion the
   parse runs on the scheduler anyway — pure OCaml holds the runtime lock,
   so a rare slow frame is the correct degradation. *)
let parse_off_thread output =
  let parse () = Diff.parse output in
  let%map.Deferred files = Cpu.in_domain ~fallback:parse parse in
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

(* ---- the branch stack ------------------------------------------------- *)

(* Cheap change signature for the poller: any local ref move shows up.
   The stack itself is fetched by [Branch_stack.fetch] — this is only the
   "did anything move?" probe that decides whether to re-run it. *)
let refs_signature () =
  match%map.Deferred
    Runner.git [ "for-each-ref"; "refs/heads"; "--format=%(refname:short)%09%(objectname)" ]
  with
  | Ok output -> output
  | Error e -> "error:" ^ Error.to_string_hum e
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
let committed ~base () =
  let range = base ^ "...HEAD" in
  let open Deferred.Or_error.Let_syntax in
  (* [--end-of-options] before the range: [base] is a branch name from a
     possibly-hostile repo (a crafted ref like [--output=path] would
     otherwise be parsed as a [git diff] option — an arbitrary file
     write). git then treats [range] strictly as a revision. *)
  let%bind raw =
    Runner.git
      [ "diff"; "--no-color"; "--raw"; "--no-abbrev"; "-M"; "--end-of-options"; range ]
  in
  let%map stats =
    Runner.git [ "diff"; "--no-color"; "--numstat"; "-M"; "--end-of-options"; range ]
  in
  let parsed = Status.parse_raw raw in
  ( List.map parsed ~f:fst
  , Diff.numstat stats |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  , List.map parsed ~f:(fun (e, blobs) -> e.Status.Entry.path, blobs)
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first) )
;;

(* The two blob sides of a committed file diff, for highlighting: the
   merge-base's content and HEAD's. *)
let file_at_base ~base path =
  let open Deferred.Or_error.Let_syntax in
  (* [--end-of-options] so a crafted branch name can't reach [merge-base]
     as an option (same hostile-repo class as [committed]). *)
  let%bind mb = Runner.git [ "merge-base"; "--end-of-options"; base; "HEAD" ] in
  Runner.git [ "show"; String.strip mb ^ ":" ^ path ]
;;

let diff_committed ~base path =
  let%bind.Deferred.Or_error output =
    Runner.git
      [ "diff"
      ; "--no-color"
      ; "-M"
      ; "--end-of-options"
      ; base ^ "...HEAD"
      ; "--"
      ; Runner.literal path
      ]
  in
  parse_off_thread output
;;

(* The three text inputs of a file diff view. THE pairing law, in one
   place, VSCode-style:
     Unstaged  = worktree vs INDEX  (old = index blob,      new = worktree file)
     Staged    = index    vs HEAD   (old = HEAD blob,       new = index blob)
     Committed = HEAD     vs BASE   (old = merge-base blob, new = HEAD blob)
   A side the file doesn't have (untracked, deleted, new since HEAD)
   reads as "". The three reads are independent; start them all before
   binding (benchmarked at ~20-40ms each — serializing them tripled the
   latency floor of every fetch). *)
let diff_with_contents side ~path =
  let or_empty d =
    d
    >>| function
    | Ok c -> c
    | Error (_ : Error.t) -> "" (* that side doesn't exist for this file *)
  in
  (* Built lazily per arm: constructing the deferred STARTS the read, and
     only the unstaged side wants the worktree file. *)
  let worktree () =
    Monitor.try_with (fun () -> Reader.file_contents path)
    >>| function
    | Ok c -> c
    | Error _ -> "" (* deleted file *)
  in
  let diff, old_content, new_content =
    match side with
    | `Unstaged -> diff_unstaged path, or_empty (file_in_index path), worktree ()
    | `Staged ->
      diff_staged path, or_empty (file_at_head path), or_empty (file_in_index path)
    | `Committed base ->
      ( diff_committed ~base path
      , or_empty (file_at_base ~base path)
      , or_empty (file_at_head path) )
  in
  let%bind.Deferred diff in
  let%bind.Deferred old_content in
  let%map.Deferred new_content in
  Or_error.map diff ~f:(fun files -> files, old_content, new_content)
;;
