open! Core

(* Selection, collapse, and viewport state over ONE file tree, pure. The
   selection/viewport laws live in [Listing] (shared with the stack pane):
   key-based selection stable under reorder, successor repair on removal
   (also what makes staging flow to the next file), wheel scrolls without
   moving the selection. This module owns only what is files-specific: the
   collapse set and the dir/file activation semantics.

   [Rows_changed] is injected by the host whenever the derived rows' keys
   change (refreshes, external mutations). *)

module Model = struct
  type t =
    { listing : Listing.Model.t
    ; collapsed : String.Set.t
    }

  let initial = { listing = Listing.Model.initial; collapsed = String.Set.empty }
end

module Action = struct
  (* Cursor-moving actions carry the pane height so the transition can
     reveal the selection (the state machine lives in Git_data, which has
     no dimensions — the pane handler does). *)
  type t =
    | Move of
        { dir : [ `Up | `Down ]
        ; height : int
        }
    | Activate of
        { row : int (* absolute row index; click: toggles directories *)
        ; height : int
        }
    | Collapse of { height : int }
      (* left: fold the dir under the selection, or jump to its parent *)
    | Expand (* right: unfold the dir under the selection *)
    | Wheel of
        { dir : int (* +1 down, -1 up *)
        ; height : int
        }
    | Rows_changed (* the derived rows changed: repair the selection *)
  [@@deriving sexp_of]
end

let row_key (row : Tree.row) =
  match row with
  | Tree.Dir { path; _ } -> path
  | File { entry; _ } -> entry.Git.Status.Entry.path
;;

let selection_key (model : Model.t) = model.listing.selection
let scroll (model : Model.t) = model.listing.scroll
let index_of rows selection = Listing.index_of ~key:row_key rows selection

let apply_action ~entries (model : Model.t) (action : Action.t) =
  let rows collapsed = Tree.rows ~entries ~collapsed in
  let current = rows model.collapsed in
  if List.is_empty current
  then { model with Model.listing = Listing.empty }
  else (
    let index = index_of current model.listing.selection in
    (* Select by index under a (possibly changed) collapse set. *)
    let select ?height ~collapsed index =
      { Model.collapsed
      ; listing = Listing.select ~key:row_key (rows collapsed) ?height ~index model.listing
      }
    in
    match action with
    | Action.Wheel { dir; height } ->
      { model with
        Model.listing =
          Listing.wheel model.listing ~total:(List.length current) ~height ~dir
      }
    | Move { dir = `Up; height } -> select ~height ~collapsed:model.collapsed (index - 1)
    | Move { dir = `Down; height } -> select ~height ~collapsed:model.collapsed (index + 1)
    | Activate { row; height } ->
      let row = Int.clamp_exn row ~min:0 ~max:(List.length current - 1) in
      (match List.nth_exn current row with
       | Tree.Dir { path; _ } ->
         let collapsed =
           if Set.mem model.collapsed path
           then Set.remove model.collapsed path
           else Set.add model.collapsed path
         in
         (* Re-locate the dir BY KEY: the toggle reshuffles indices. *)
         select ~height ~collapsed (index_of (rows collapsed) (Some path))
       | File _ -> select ~height ~collapsed:model.collapsed row)
    | Expand ->
      (match List.nth_exn current index with
       | Tree.Dir { path; expanded = false; _ } ->
         let collapsed = Set.remove model.collapsed path in
         select ~collapsed (index_of (rows collapsed) (Some path))
       | Dir _ | File _ -> model)
    | Collapse { height } ->
      (match List.nth_exn current index with
       | Tree.Dir { path; expanded = true; _ } ->
         let collapsed = Set.add model.collapsed path in
         select ~height ~collapsed (index_of (rows collapsed) (Some path))
       | Dir _ | File _ ->
         (match Listing.parent_index ~depth:Tree.depth current index with
          | Some i -> select ~height ~collapsed:model.collapsed i
          | None -> model))
    | Rows_changed ->
      { model with Model.listing = Listing.rows_changed ~key:row_key current model.listing })
;;

(* The file under the selection, if any — a directory row selects
   nothing. *)
let selection rows ~cursor =
  match List.nth rows cursor with
  | Some (Tree.File { entry; _ }) -> Some entry.Git.Status.Entry.path
  | Some (Dir _) | None -> None
;;
