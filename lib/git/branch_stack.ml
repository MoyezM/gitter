open! Core

(* The branch stack, derived from git ALONE (no Graphite metadata): local
   branch heads plus the commit DAG above their common base. Each branch's
   parent is inferred as the branch with the DEEPEST merge-base into its
   ancestry, where a branch's "ancestry" for matching purposes includes
   its recent REFLOG tips: after an amend the parent's old tip leaves its
   current history, but the child still sits on it and the reflog still
   names it — exactly the association gt records explicitly. "Needs
   restack" then falls out as: the parent's CURRENT head is no longer in
   the child's ancestry. Total: malformed lines are dropped, cycles
   guarded, never raises.

   The DAG is only the commits ABOVE the base (rev-list --branches --not
   base) — a commit absent from the DAG is at-or-below the base and
   therefore an ancestor of every branch. *)

module Branch = struct
  type t =
    { name : string
    ; depth : int (* 0 = trunk *)
    ; is_current : bool
    ; is_trunk : bool
    ; needs_restack : bool
    }
  [@@deriving sexp_of, equal]
end

(* One for-each-ref line: "<branch>\t<sha>" (tabs cannot appear in
   refnames). *)
let parse_head line =
  match String.split line ~on:'\t' with
  | [ name; sha ] when (not (String.is_empty name)) && not (String.is_empty sha) ->
    Some (name, sha)
  | _ -> None
;;

(* One rev-list --parents line: "<sha> <parent> <parent>...". *)
let parse_dag_line line =
  match String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) with
  | sha :: parents -> Some (sha, parents)
  | [] -> None
;;

let parse ~heads ~dag ~reflogs ~trunk ~current =
  (* The heads INPUT ORDER carries recency: the caller lists refs sorted
     by -committerdate, so line number = how recently a branch moved. *)
  let recency =
    String.split_lines heads
    |> List.filter_map ~f:parse_head
    |> List.mapi ~f:(fun i (name, _) -> name, i)
    |> String.Map.of_alist_reduce ~f:Int.min
  in
  let heads =
    String.split_lines heads
    |> List.filter_map ~f:parse_head
    |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  (* "name\tsha" lines, a branch's recent reflog tips (head included). *)
  let reflog_tips =
    String.split_lines reflogs
    |> List.filter_map ~f:parse_head
    |> String.Map.of_alist_multi
  in
  match Map.find heads trunk with
  | None -> []
  | Some trunk_sha ->
    let dag =
      String.split_lines dag
      |> List.filter_map ~f:parse_dag_line
      |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
    in
    (* Ancestors of [sha] within the DAG, including [sha] itself when it is
       in the DAG. Iterative: stacks can sit on long histories. *)
    let ancestors sha =
      let seen = ref String.Set.empty in
      let frontier = ref [ sha ] in
      while not (List.is_empty !frontier) do
        match !frontier with
        | [] -> ()
        | c :: rest ->
          frontier := rest;
          if Map.mem dag c && not (Set.mem !seen c)
          then (
            seen := Set.add !seen c;
            frontier := Map.find_exn dag c @ !frontier)
      done;
      !seen
    in
    let branch_ancestors =
      Map.map heads ~f:ancestors (* name -> its head's DAG ancestry *)
    in
    (* Longest-path height of a DAG commit above the base; memoized.
       Marking before recursing keeps a corrupt cyclic input terminating. *)
    let height =
      let memo = ref String.Map.empty in
      let rec go sha =
        match Map.find !memo sha with
        | Some h -> h
        | None ->
          (match Map.find dag sha with
           | None -> -1 (* at or below the base *)
           | Some parents ->
             memo := Map.set !memo ~key:sha ~data:(-1);
             let h = 1 + List.fold parents ~init:(-1) ~f:(fun acc p -> Int.max acc (go p)) in
             memo := Map.set !memo ~key:sha ~data:h;
             h)
      in
      go
    in
    let trunk_ancestry = Map.find_exn branch_ancestors trunk in
    (* Fully merged branches (tip strictly below the trunk head) are not
       part of any stack — hide them, except the checked-out branch. *)
    let is_current name =
      match current with
      | Some c -> String.equal c name
      | None -> false
    in
    let live =
      Map.filteri heads ~f:(fun ~key:name ~data:sha ->
        String.equal name trunk
        || is_current name
        || not (Set.mem trunk_ancestry sha && not (String.equal sha trunk_sha)))
    in
    (* A branch's TIPS for matching: its head plus recent reflog tips. The
       reflog part is what re-attaches a child to an amended parent. *)
    let tips =
      Map.mapi heads ~f:(fun ~key:name ~data:sha ->
        sha :: (Map.find reflog_tips name |> Option.value ~default:[]))
    in
    (* Parent of [b]: the branch whose TIP sits deepest in [b]'s ancestry
       — a true ancestor matches at its current head, an amended parent at
       its old (reflog) tip. Matching only at TIPS (and never at [b]'s own
       head) is what stops descendants from claiming parenthood: a child
       contains [b]'s head, but none of the child's tips lie in [b]'s
       history. No tip in the DAG means everything common sits at/below
       the base: attach to the trunk. *)
    let parent_of b b_sha =
      if String.equal b trunk
      then None
      else (
        let b_anc = Set.add (Map.find_exn branch_ancestors b) b_sha in
        let best =
          Map.fold live ~init:None ~f:(fun ~key:p ~data:p_sha best ->
            let candidate =
              if String.equal p b
              then None
              else if String.equal p_sha b_sha
              then
                (* Two branches on the same commit ABOVE the base: pick a
                   deterministic direction so they can't be each other's
                   parent. At the base itself (height < 0) there is no
                   stacking relation — a pile of fresh branches on the
                   trunk head must all be trunk children, not an
                   alphabetical chain. *)
                if String.( < ) p b && height b_sha >= 0
                then Some (height b_sha, true)
                else None
              else (
                let matched =
                  Map.find_exn tips p
                  |> List.filter ~f:(fun tip ->
                    (not (String.equal tip b_sha)) && Set.mem b_anc tip)
                in
                match matched with
                | [] -> None
                | _ :: _ ->
                  let h =
                    List.fold matched ~init:(-1) ~f:(fun acc t -> Int.max acc (height t))
                  in
                  Some (h, Set.mem b_anc p_sha))
            in
            match candidate, best with
            | None, best -> best
            | Some (h, ta), Some ((_, best_h, best_ta) as kept)
              when best_h > h || (best_h = h && Bool.( >= ) best_ta ta) -> Some kept
            | Some (h, ta), _ -> Some (p, h, ta))
        in
        match best with
        | Some (p, _, _) -> Some p
        | None -> Some trunk)
    in
    let parents =
      Map.mapi live ~f:(fun ~key:name ~data:sha -> parent_of name sha)
    in
    (* Matching at reflog TIPS is what re-attaches a child to an amended
       parent, but it also costs the antisymmetry that head shas alone
       guarantee: after enough rebasing two branches can each hold a tip
       inside the other's history, and [parent_of] then names each the
       other's parent. Reattach exactly the branches ON such a cycle to the
       trunk, so [parents] is always a forest rooted there — a branch that
       merely sits above a cycle keeps its real parent, which becomes valid
       again once the cycle is cut. Everything downstream depends on this:
       the sibling sort walks CHILDREN with no visited set of its own (so a
       cycle is unbounded recursion), and [walk]'s guard would terminate but
       drop the entire cycle from the display. *)
    let parents =
      let on_cycle name =
        (* Following [name]'s parents leads back to [name] itself. Stopping
           on any repeat keeps a branch that merely runs INTO someone else's
           loop off the list. *)
        let rec go n seen =
          match Map.find parents n |> Option.join with
          | None -> false
          | Some p ->
            if String.equal p name
            then true
            else if Set.mem seen p
            then false
            else go p (Set.add seen p)
        in
        go name (String.Set.singleton name)
      in
      Map.mapi parents ~f:(fun ~key:name ~data:parent ->
        if on_cycle name then Some trunk else parent)
    in
    let children =
      Map.fold parents ~init:String.Map.empty ~f:(fun ~key:child ~data:parent acc ->
        match parent with
        | None -> acc
        | Some p -> Map.add_multi acc ~key:p ~data:child)
      |> Map.map ~f:(List.sort ~compare:String.compare)
    in
    (* Sibling order: the subtree carrying the CURRENT branch first (your
       working context beats everything), then by subtree recency — a
       child's activity bubbles up to its parent — then name. *)
    let rec subtree_has_current name =
      is_current name
      || List.exists
           (Map.find children name |> Option.value ~default:[])
           ~f:subtree_has_current
    in
    let rec subtree_rank name =
      List.fold
        (Map.find children name |> Option.value ~default:[])
        ~init:(Map.find recency name |> Option.value ~default:Int.max_value)
        ~f:(fun acc child -> Int.min acc (subtree_rank child))
    in
    let children =
      Map.map children ~f:(fun kids ->
        List.sort kids ~compare:(fun a b ->
          match subtree_has_current a, subtree_has_current b with
          | true, false -> -1
          | false, true -> 1
          | _ ->
            (match Int.compare (subtree_rank a) (subtree_rank b) with
             | 0 -> String.compare a b
             | c -> c)))
    in
    let needs_restack name =
      match Map.find parents name |> Option.join with
      | None -> false
      | Some parent ->
        let parent_sha = Map.find_exn live parent in
        let anc = Set.add (Map.find_exn branch_ancestors name) (Map.find_exn live name) in
        (* Parent head absent from the DAG = at/below the base = an
           ancestor of everything: up to date. *)
        Map.mem dag parent_sha && not (Set.mem anc parent_sha)
    in
    (* Depth-first from the trunk: the tree grows DOWN like the files
       tree — trunk first, children indented below, the current stack
       leading its siblings. *)
    let visited = ref String.Set.empty in
    let rec walk name depth =
      if Set.mem !visited name
      then []
      else (
        visited := Set.add !visited name;
        { Branch.name
        ; depth
        ; is_current = is_current name
        ; is_trunk = String.equal name trunk
        ; needs_restack = needs_restack name
        }
        :: List.concat_map
             (Map.find children name |> Option.value ~default:[])
             ~f:(fun child -> walk child (depth + 1)))
    in
    walk trunk 0
;;

(* The current branch's parent in a parsed stack: the nearest earlier
   entry one depth shallower (display order is DFS, trunk first). *)
let parent_of_current (branches : Branch.t list) =
  match List.findi branches ~f:(fun _ b -> b.is_current) with
  | None -> None
  | Some (i, current) ->
    if current.depth = 0
    then None
    else
      List.filter_mapi branches ~f:(fun j b ->
        Option.some_if (j < i && b.depth = current.depth - 1) b.name)
      |> List.last
;;

(* ---- fetching (the git pipeline that feeds [parse]) -------------------- *)

(* Bounded so a branch far behind trunk can't make the DAG walk huge; the
   parser degrades gracefully when truncated (distant branches attach to
   the trunk). *)
let dag_commit_cap = 5000
let reflog_tip_cap = 20
let reflog_branch_cap = 40

let fetch () =
  let open Async in
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
    parse ~heads:heads_raw ~dag ~reflogs ~trunk ~current
;;
