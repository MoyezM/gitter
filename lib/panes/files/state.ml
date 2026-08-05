open! Core

(* Selection is a KEY (the row's path), not an index: the rows are just a
   flattened traversal of the tree, and a stored index silently retargets
   whenever the data reorders or shrinks underneath it. The law here:
   selection is STABLE under reorder; when the selected row disappears it
   moves to the nearest surviving SUCCESSOR in the old traversal (then
   predecessor) — computed by [repair], a pure function over the old and
   new key orders. That one rule also gives the rapid-staging flow: stage
   a file and the selection lands on the next one.

   [Rows_changed] is injected by the host whenever the derived rows'
   keys change (refreshes, external mutations); every transition stores
   the current key order so the NEXT change can repair against it.

   Viewport contract shared with the other panes: the wheel scrolls
   without moving the selection; selection motion reveals with minimal
   scroll. *)

module Model = struct
  type t =
    { selection : string option (* row key; None = nothing chosen yet *)
    ; collapsed : String.Set.t
    ; scroll : int
    ; keys : string list (* the rows' keys at the last transition *)
    }

  let initial = { selection = None; collapsed = String.Set.empty; scroll = 0; keys = [] }
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

(* Same flat step as the diff pane. *)
let wheel_step = 3

(* One owner for the viewport mapping: render and the click handler must
   agree on it or clicks activate the wrong row. *)
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

(* The selection's position in the current rows; a missing/unset key reads
   as the first row, so movement is total and a fresh pane starts at the
   top. *)
let index_of rows selection =
  match selection with
  | None -> 0
  | Some key ->
    List.findi rows ~f:(fun _ r -> String.equal (row_key r) key)
    |> Option.value_map ~default:0 ~f:fst
;;

(* The repair law lives in [Selection] — shared with the stack pane. *)
let repair = Selection.repair

(* The nearest preceding row shallower than [i] — its enclosing dir. *)
let parent rows ~index =
  match List.nth rows index with
  | None -> None
  | Some row ->
    let d = Tree.depth row in
    if d = 0
    then None
    else
      List.filter_mapi rows ~f:(fun i r -> Option.some_if (i < index && Tree.depth r < d) i)
      |> List.last
;;

let clamp rows c = Int.clamp_exn c ~min:0 ~max:(Int.max 0 (List.length rows - 1))

let apply_action ~entries (model : Model.t) (action : Action.t) =
  let rows collapsed = Tree.rows ~entries ~collapsed in
  let current_rows = rows model.collapsed in
  if List.is_empty current_rows
  then { model with Model.selection = None; scroll = 0; keys = [] }
  else (
    (* Finish: re-derive rows under the (possibly changed) collapse set,
       select by index, snapshot keys, reveal. *)
    let finish ?height ~collapsed ~index (model : Model.t) =
      let rows = rows collapsed in
      let index = clamp rows index in
      let selection = List.nth rows index |> Option.map ~f:row_key in
      let scroll =
        match height with
        | None -> offset ~total:(List.length rows) ~height:1 model.scroll
        | Some height -> reveal ~height ~cursor:index model.scroll
      in
      { Model.selection; collapsed; scroll; keys = List.map rows ~f:row_key }
    in
    let index = index_of current_rows model.selection in
    match action with
    | Action.Wheel { dir; height } ->
      let limit = Int.max 0 (List.length current_rows - Int.max 1 height) in
      let scroll =
        Int.clamp_exn
          (offset ~total:(List.length current_rows) ~height:1 model.scroll
           + (wheel_step * dir))
          ~min:0
          ~max:limit
      in
      { model with Model.scroll; keys = List.map current_rows ~f:row_key }
    | Move { dir = `Up; height } ->
      finish ~height ~collapsed:model.collapsed ~index:(index - 1) model
    | Move { dir = `Down; height } ->
      finish ~height ~collapsed:model.collapsed ~index:(index + 1) model
    | Activate { row; height } ->
      let row = clamp current_rows row in
      (match List.nth_exn current_rows row with
       | Tree.Dir { path; _ } ->
         let collapsed =
           if Set.mem model.collapsed path
           then Set.remove model.collapsed path
           else Set.add model.collapsed path
         in
         (* Select the dir BY KEY: the toggle reshuffles indices. *)
         let rows' = rows collapsed in
         finish ~height ~collapsed ~index:(index_of rows' (Some path)) model
       | File _ -> finish ~height ~collapsed:model.collapsed ~index:row model)
    | Expand ->
      (match List.nth_exn current_rows index with
       | Tree.Dir { path; expanded = false; _ } ->
         let collapsed = Set.remove model.collapsed path in
         let rows' = rows collapsed in
         finish ~collapsed ~index:(index_of rows' (Some path)) model
       | Dir _ | File _ -> model)
    | Collapse { height } ->
      (match List.nth_exn current_rows index with
       | Tree.Dir { path; expanded = true; _ } ->
         let collapsed = Set.add model.collapsed path in
         let rows' = rows collapsed in
         finish ~height ~collapsed ~index:(index_of rows' (Some path)) model
       | Dir _ | File _ ->
         (match parent current_rows ~index with
          | Some i -> finish ~height ~collapsed:model.collapsed ~index:i model
          | None -> model))
    | Rows_changed ->
      let new_keys = List.map current_rows ~f:row_key in
      let selection = repair ~old_keys:model.keys ~selection:model.selection ~new_keys in
      { model with
        Model.selection
      ; keys = new_keys
      ; scroll = offset ~total:(List.length current_rows) ~height:1 model.scroll
      })
;;

(* The file under the selection, if any — a directory row selects
   nothing. *)
let selection rows ~cursor =
  match List.nth rows cursor with
  | Some (Tree.File { entry; _ }) -> Some entry.Git.Status.Entry.path
  | Some (Dir _) | None -> None
;;
