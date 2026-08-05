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
  type t =
    { listing : Listing.Model.t
    ; overrides : bool String.Map.t (* row key -> collapsed, user-toggled *)
    }

  let initial = { listing = Listing.Model.initial; overrides = String.Map.empty }
end

module Action = struct
  (* Height rides the actions: the state machine has no dimensions (see
     the files pane precedent). *)
  type t =
    | Move of
        { dir : [ `Up | `Down ]
        ; height : int
        }
    | Activate of
        { row : int (* absolute visible-row index *)
        ; height : int
        }
    | Fold of { height : int } (* h: collapse, or jump to the parent *)
    | Unfold of { height : int } (* l *)
    | Wheel of
        { dir : int
        ; height : int
        }
    | Rows_changed (* the derived rows changed: repair the selection *)
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

let visible ~(branches : Git.Branch_stack.Branch.t list) ~overrides : Row.t list =
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
    let collapsed = collapsed_of node ~depth ~on_chain in
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

let row_key (r : Row.t) = r.key
let selection_key (model : Model.t) = model.listing.selection
let scroll (model : Model.t) = model.listing.scroll
let index_of rows selection = Listing.index_of ~key:row_key rows selection

let apply_action ~branches (model : Model.t) (action : Action.t) =
  let rows overrides = visible ~branches ~overrides in
  let current = rows model.overrides in
  if List.is_empty current
  then { model with Model.listing = Listing.empty }
  else (
    let index = index_of current model.listing.selection in
    (* Select by index under (possibly changed) fold overrides. *)
    let select ?height ~overrides index =
      { Model.overrides
      ; listing = Listing.select ~key:row_key (rows overrides) ?height ~index model.listing
      }
    in
    match action with
    | Action.Wheel { dir; height } ->
      { model with
        Model.listing =
          Listing.wheel model.listing ~total:(List.length current) ~height ~dir
      }
    | Move { dir = `Up; height } -> select ~height ~overrides:model.overrides (index - 1)
    | Move { dir = `Down; height } -> select ~height ~overrides:model.overrides (index + 1)
    | Activate { row; height } ->
      let row = Int.clamp_exn row ~min:0 ~max:(List.length current - 1) in
      let r = List.nth_exn current row in
      if r.has_children
      then (
        let overrides = Map.set model.overrides ~key:r.key ~data:(not r.collapsed) in
        select ~height ~overrides (index_of (rows overrides) (Some r.key)))
      else select ~height ~overrides:model.overrides row
    | Fold { height } ->
      let r = List.nth_exn current index in
      if r.has_children && not r.collapsed
      then (
        let overrides = Map.set model.overrides ~key:r.key ~data:true in
        select ~height ~overrides (index_of (rows overrides) (Some r.key)))
      else (
        match Listing.parent_index ~depth:(fun (r : Row.t) -> r.depth) current index with
        | Some i -> select ~height ~overrides:model.overrides i
        | None -> model)
    | Unfold { height } ->
      let r = List.nth_exn current index in
      if r.has_children && r.collapsed
      then (
        let overrides = Map.set model.overrides ~key:r.key ~data:false in
        select ~height ~overrides (index_of (rows overrides) (Some r.key)))
      else model
    | Rows_changed ->
      { model with Model.listing = Listing.rows_changed ~key:row_key current model.listing })
;;
