open! Core

(* Cursor, fold, and viewport state over the branch stack, pure.

   The flat Branch_stack list (DFS order: trunk first, the tree grows
   down) is rebuilt into a tree, and sibling branches sharing a slash
   prefix (codex/..., devin/...) are gathered under synthesized GROUP
   rows — a work repo's noise is mostly flat leaf branches, which
   per-branch folding alone can't shrink. Fold defaults keep exactly the
   active stack readable: groups and off-chain top-level branches start
   collapsed; user toggles are stored as per-key overrides so they
   survive refreshes.

   Same viewport contract as the other panes: the wheel scrolls without
   moving the cursor; cursor motion reveals with minimal scroll. *)

module Row = struct
  type kind =
    | Branch of Git.Branch_stack.Branch.t
    | Group of { prefix : string }

  type t =
    { kind : kind
    ; key : string (* fold-override key *)
    ; depth : int (* display depth (groups add a level) *)
    ; has_children : bool
    ; collapsed : bool (* only meaningful when has_children *)
    ; hidden : int (* branches hidden under a collapsed row *)
    ; on_current_stack : bool (* on the current branch's chain *)
    }
end

module Model = struct
  (* fold = the per-key collapse overrides (row key -> collapsed). *)
  type t = bool String.Map.t Search.Tree_search.Model.t

  let initial : t = Search.Tree_search.Model.initial String.Map.empty
end

module Action = struct
  (* The navigation vocabulary is [Tree_listing]'s; only what is
     stack-specific stays here. *)
  type t =
    | Nav of Tree_listing.Action.t
    | Enter of { height : int }
      (* set the selected branch as the diff base (resolved at APPLY time
         by the host wrapper - burst-safe); on a group row: toggle it *)
  [@@deriving sexp_of]
end

(* ---- tree building ----------------------------------------------------- *)

type node =
  | Branch_node of
      { branch : Git.Branch_stack.Branch.t
      ; children : node list
      }
  | Group_node of
      { key : string
      ; prefix : string
      ; children : node list
      }

(* The flat list back into a forest; input is already DFS order. *)
let rec forest ~depth (items : Git.Branch_stack.Branch.t list)
  : node list * Git.Branch_stack.Branch.t list
  =
  match items with
  | b :: rest when b.depth = depth ->
    let children, rest = forest ~depth:(depth + 1) rest in
    let siblings, rest = forest ~depth rest in
    Branch_node { branch = b; children = group ~parent:b.name children } :: siblings, rest
  | _ -> [], items

(* Gather sibling branches sharing a slash prefix (>= 2 of them) under a
   group node, at the first member's position. *)
and group ~parent (children : node list) : node list =
  let prefix_of = function
    | Group_node _ -> None
    | Branch_node { branch; _ } ->
      Option.map (String.lsplit2 branch.name ~on:'/') ~f:fst
  in
  let counts =
    List.filter_map children ~f:prefix_of
    |> List.fold ~init:String.Map.empty ~f:(fun acc p ->
      Map.update acc p ~f:(fun c -> 1 + Option.value c ~default:0))
  in
  let grouped p = match Map.find counts p with Some c -> c >= 2 | None -> false in
  let seen = ref String.Set.empty in
  List.filter_map children ~f:(fun child ->
    match prefix_of child with
    | Some p when grouped p ->
      if Set.mem !seen p
      then None
      else (
        seen := Set.add !seen p;
        let members =
          List.filter children ~f:(fun c ->
            match prefix_of c with
            | Some q -> String.equal p q
            | None -> false)
        in
        Some (Group_node { key = parent ^ "//" ^ p; prefix = p; children = members }))
    | _ -> Some child)
;;

let children_of = function
  | Branch_node { children; _ } -> children
  | Group_node { children; _ } -> children
;;

let rec contains_current node =
  match node with
  | Branch_node { branch; children } ->
    branch.is_current || List.exists children ~f:contains_current
  | Group_node { children; _ } -> List.exists children ~f:contains_current
;;

let rec branch_count node =
  match node with
  | Branch_node { children; _ } -> 1 + List.fold children ~init:0 ~f:(fun a c -> a + branch_count c)
  | Group_node { children; _ } -> List.fold children ~init:0 ~f:(fun a c -> a + branch_count c)
;;

(* ---- the visible rows -------------------------------------------------- *)

let visible' ~(branches : Git.Branch_stack.Branch.t list) ~overrides ~bypass_folds
  : Row.t list
  =
  let roots, _leftover = forest ~depth:0 branches in
  let collapsed_of node ~depth ~on_chain =
    let key, default =
      match node with
      | Group_node { key; _ } -> key, not (contains_current node)
      | Branch_node { branch; _ } ->
        branch.name, depth = 1 && (not on_chain) && not (contains_current node)
    in
    Map.find overrides key |> Option.value ~default
  in
  (* DFS trunk-first — the display order. [in_subtree] marks the current
     branch's own subtree; a node containing current is on its ancestor
     path. *)
  let rec walk node ~depth ~in_subtree acc =
    let key =
      match node with
      | Group_node { key; _ } -> key
      | Branch_node { branch; _ } -> branch.name
    in
    let on_chain = in_subtree || contains_current node in
    let children = children_of node in
    let collapsed = (not bypass_folds) && collapsed_of node ~depth ~on_chain in
    let has_children = not (List.is_empty children) in
    let hidden = if collapsed && has_children then branch_count node - (match node with Branch_node _ -> 1 | Group_node _ -> 0) else 0 in
    let row =
      { Row.kind =
          (match node with
           | Branch_node { branch; _ } -> Row.Branch branch
           | Group_node { prefix; _ } -> Row.Group { prefix })
      ; key
      ; depth
      ; has_children
      ; collapsed
      ; hidden
      ; on_current_stack = on_chain
      }
    in
    let acc = row :: acc in
    if collapsed
    then acc
    else (
      let in_subtree =
        in_subtree
        ||
        match node with
        | Branch_node { branch; _ } -> branch.is_current
        | Group_node _ -> in_subtree
      in
      List.fold children ~init:acc ~f:(fun acc c -> walk c ~depth:(depth + 1) ~in_subtree acc))
  in
  (* [walk] conses (reversed DFS); flip back to DFS for display. *)
  List.rev (List.fold roots ~init:[] ~f:(fun acc r -> walk r ~depth:0 ~in_subtree:false acc))
;;

let visible ~branches ~overrides = visible' ~branches ~overrides ~bypass_folds:false

(* Every row with every fold open — the space search filters (T2) and
   [n]/[N] jump over (T7). *)
let all_rows ~branches =
  visible' ~branches ~overrides:String.Map.empty ~bypass_folds:true
;;

let row_key (r : Row.t) = r.key
let selection_key (model : Model.t) = model.listing.selection
let scroll (model : Model.t) = model.listing.scroll
let index_of rows selection = Listing.index_of ~key:row_key rows selection

(* The fold keys above [index] - what revealing a row must unfold.
   Iterated [Listing.parent_index], so ancestry has one owner: the same
   walk the fold-jump uses. *)
let ancestor_keys rows index =
  let rec up index acc =
    match Listing.parent_index ~depth:(fun (r : Row.t) -> r.depth) rows index with
    | None -> acc
    | Some i -> up i ((List.nth_exn rows i).Row.key :: acc)
  in
  up index []
;;

(* The row facts and the reveal that make this pane searchable: only
   BRANCH rows are match candidates, on their names (T1); groups are
   ancestor-chain carriers; every row can parent (branches parent
   branches); revealing unfolds the ancestor chain via overrides. *)
let pane ~branches : (Row.t, bool String.Map.t) Search.Tree_search.pane =
  { rows = (fun overrides -> visible ~branches ~overrides)
  ; all_rows = (fun () -> all_rows ~branches)
  ; key = row_key
  ; depth = (fun (r : Row.t) -> r.depth)
  ; is_parent = (fun (_ : Row.t) -> true)
  ; candidate =
      (fun r ->
        match r.Row.kind with
        | Row.Branch b -> Some b.name
        | Group _ -> None)
  ; reveal =
      (fun overrides ~key ->
        let full = all_rows ~branches in
        match List.findi full ~f:(fun (_ : int) r -> String.equal r.Row.key key) with
        | None -> overrides
        | Some (index, _) ->
          List.fold (ancestor_keys full index) ~init:overrides ~f:(fun acc key ->
            Map.set acc ~key ~data:false))
  }
;;

let displayed_rows ~branches ~overrides ~search =
  Search.Tree_search.displayed_rows (pane ~branches) ~fold:overrides ~search
;;

let visible_rows ~branches (model : Model.t) =
  displayed_rows ~branches ~overrides:model.fold ~search:model.search
;;

(* What folding means here: rows with children fold via the per-key
   override map. *)
let caps ~branches : (Row.t, bool String.Map.t) Tree_listing.caps =
  { pane = pane ~branches
  ; fold_state =
      (fun (r : Row.t) ->
        if r.has_children then if r.collapsed then `Closed else `Open else `Leaf)
  ; set_folded = (fun overrides ~key ~folded -> Map.set overrides ~key ~data:folded)
  }
;;

(* The pane's own transitions, over already-RESOLVED actions —
   [Search.Tree_search.apply] owns the mode dispatch and the search
   lifecycle; [Tree_listing.apply] owns navigation. *)
let apply_plain ~branches (model : Model.t) (action : Action.t) =
  match action with
  | Action.Nav nav -> Tree_listing.apply (caps ~branches) model nav
  | Enter { height } ->
    (* Group rows fold-toggle; branch resolution happens in the host
       wrapper, which schedules set-base for the branch case. *)
    let current = visible_rows ~branches model in
    let index = index_of current model.listing.selection in
    (match List.nth current index with
     | Some { Row.kind = Group _; has_children = true; _ } ->
       Tree_listing.apply (caps ~branches) model (Activate { row = index; height })
     | Some _ | None -> model)
;;

let apply_action ~branches (model : Model.t) (action : Action.t Search.Action.t) =
  Search.Tree_search.apply (pane ~branches) ~apply_pane:(apply_plain ~branches) model action
;;

(* The pane's bottom-border search line, and its counter (exposed for
   tests). *)
let match_counts ~branches search = Search.Tree_search.match_counts (pane ~branches) search

let border ~branches search ~width =
  Search.Tree_search.border (pane ~branches) ~search ~width
;;

