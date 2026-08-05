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
    { selection : string option (* row KEY; stable under reorder *)
    ; scroll : int
    ; overrides : bool String.Map.t (* row key -> collapsed, user-toggled *)
    ; keys : string list (* visible rows' keys at the last transition *)
    }

  let initial =
    { selection = None; scroll = 0; overrides = String.Map.empty; keys = [] }
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

let wheel_step = 3

let offset ~total ~height scroll =
  Int.max 0 (Int.min scroll (total - Int.max 1 height))
;;

let reveal ~height ~cursor scroll =
  if height <= 0
  then scroll
  else if cursor < scroll
  then cursor
  else if cursor >= scroll + height
  then cursor - height + 1
  else scroll
;;

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

let clamp rows c = Int.clamp_exn c ~min:0 ~max:(Int.max 0 (List.length rows - 1))

(* The visible parent of visible row [i]: the nearest EARLIER visible row
   one depth shallower (the tree grows down). *)
let parent_index (rows : Row.t list) i =
  match List.nth rows i with
  | None -> None
  | Some row ->
    List.filter_mapi rows ~f:(fun j (r : Row.t) ->
      Option.some_if (j < i && r.depth = row.depth - 1) j)
    |> List.last
;;

(* The selection's position in the current rows; a missing/unset key reads
   as the first row. *)
let index_of (rows : Row.t list) selection =
  match selection with
  | None -> 0
  | Some key ->
    List.findi rows ~f:(fun _ (r : Row.t) -> String.equal r.key key)
    |> Option.value_map ~default:0 ~f:fst
;;

let apply_action ~branches (model : Model.t) (action : Action.t) =
  let rows overrides = visible ~branches ~overrides in
  let current = rows model.overrides in
  if List.is_empty current
  then { model with Model.selection = None; scroll = 0; keys = [] }
  else (
    let keys_of rows = List.map rows ~f:(fun (r : Row.t) -> r.key) in
    (* Finish: re-derive rows under the (possibly changed) overrides,
       select by index, snapshot keys, reveal. *)
    let finish ?height ~overrides ~index (model : Model.t) =
      let rows = rows overrides in
      let index = clamp rows index in
      let selection = List.nth rows index |> Option.map ~f:(fun (r : Row.t) -> r.key) in
      let scroll =
        match height with
        | None -> offset ~total:(List.length rows) ~height:1 model.scroll
        | Some height -> reveal ~height ~cursor:index model.scroll
      in
      { Model.selection; overrides; scroll; keys = keys_of rows }
    in
    let index = index_of current model.selection in
    match action with
    | Action.Wheel { dir; height } ->
      let limit = Int.max 0 (List.length current - Int.max 1 height) in
      let scroll =
        Int.clamp_exn
          (offset ~total:(List.length current) ~height:1 model.scroll + (wheel_step * dir))
          ~min:0
          ~max:limit
      in
      { model with Model.scroll = scroll; keys = keys_of current }
    | Move { dir = `Up; height } ->
      finish ~height ~overrides:model.overrides ~index:(index - 1) model
    | Move { dir = `Down; height } ->
      finish ~height ~overrides:model.overrides ~index:(index + 1) model
    | Activate { row; height } ->
      let row = clamp current row in
      let r = List.nth_exn current row in
      if r.has_children
      then (
        let overrides = Map.set model.overrides ~key:r.key ~data:(not r.collapsed) in
        finish ~height ~overrides ~index:(index_of (rows overrides) (Some r.key)) model)
      else finish ~height ~overrides:model.overrides ~index:row model
    | Fold { height } ->
      let r = List.nth_exn current index in
      if r.has_children && not r.collapsed
      then (
        let overrides = Map.set model.overrides ~key:r.key ~data:true in
        finish ~height ~overrides ~index:(index_of (rows overrides) (Some r.key)) model)
      else (
        match parent_index current index with
        | Some i -> finish ~height ~overrides:model.overrides ~index:i model
        | None -> model)
    | Unfold { height } ->
      let r = List.nth_exn current index in
      if r.has_children && r.collapsed
      then (
        let overrides = Map.set model.overrides ~key:r.key ~data:false in
        finish ~height ~overrides ~index:(index_of (rows overrides) (Some r.key)) model)
      else model
    | Rows_changed ->
      let new_keys = keys_of current in
      let selection =
        Selection.repair ~old_keys:model.keys ~selection:model.selection ~new_keys
      in
      { model with
        Model.selection
      ; keys = new_keys
      ; scroll = offset ~total:(List.length current) ~height:1 model.scroll
      })
;;
