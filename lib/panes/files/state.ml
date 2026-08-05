open! Core

module Model = struct
  type t =
    { cursor : int
    ; collapsed : String.Set.t
    ; scroll : int (* first visible row; the wheel moves it freely *)
    }

  let initial = { cursor = 0; collapsed = String.Set.empty; scroll = 0 }
end

module Action = struct
  (* Cursor-moving actions carry the pane height so the transition can
     reveal the cursor (the state machine lives in Git_data, which has no
     dimensions — the pane handler does). *)
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
      (* left: fold the dir under the cursor, or jump to its parent *)
    | Expand (* right: unfold the dir under the cursor *)
    | Wheel of
        { dir : int (* +1 down, -1 up *)
        ; height : int
        }
  [@@deriving sexp_of]
end

let clamp rows c = Int.clamp_exn c ~min:0 ~max:(Int.max 0 (List.length rows - 1))

(* Same flat step as the diff pane: velocity comes from the terminal's
   event rate, so a fixed per-tick step already scrolls faster on faster
   flicks. *)
let wheel_step = 3

(* One owner for the viewport mapping: render and the click handler must
   agree on it or clicks activate the wrong row. *)
let offset ~total ~height scroll =
  Int.max 0 (Int.min scroll (total - Int.max 1 height))
;;

(* Minimal-motion reveal: scroll only when the cursor left the viewport.
   Clicking a visible row therefore never moves the view; keyboard motion
   scrolls one row at a time at the edges. *)
let reveal ~height ~cursor scroll =
  if height <= 0
  then scroll
  else if cursor < scroll
  then cursor
  else if cursor >= scroll + height
  then cursor - height + 1
  else scroll
;;

(* The nearest preceding row shallower than the cursor's — its enclosing
   directory. *)
let parent rows ~cursor =
  match List.nth rows cursor with
  | None -> None
  | Some row ->
    let d = Tree.depth row in
    if d = 0
    then None
    else
      List.filter_mapi rows ~f:(fun i r -> Option.some_if (i < cursor && Tree.depth r < d) i)
      |> List.last
;;

let apply_action ~entries (model : Model.t) (action : Action.t) =
  let rows = Tree.rows ~entries ~collapsed:model.collapsed in
  if List.is_empty rows
  then { model with Model.cursor = 0; scroll = 0 }
  else (
    let cursor = clamp rows model.cursor in
    let scroll = offset ~total:(List.length rows) ~height:1 model.scroll in
    let model = { model with Model.cursor = cursor; scroll } in
    let follow ~height (model : Model.t) =
      { model with Model.scroll = reveal ~height ~cursor:model.cursor model.scroll }
    in
    match action with
    | Action.Wheel { dir; height } ->
      (* Viewport only — the selection stays put, off-screen if need be. *)
      let limit = Int.max 0 (List.length rows - Int.max 1 height) in
      { model with
        Model.scroll = Int.clamp_exn (scroll + (wheel_step * dir)) ~min:0 ~max:limit
      }
    | Move { dir = `Up; height } ->
      follow ~height { model with Model.cursor = clamp rows (cursor - 1) }
    | Move { dir = `Down; height } ->
      follow ~height { model with Model.cursor = clamp rows (cursor + 1) }
    | Activate { row; height } ->
      let cursor = clamp rows row in
      follow
        ~height
        (match List.nth_exn rows cursor with
         | Tree.Dir { path; _ } ->
           { model with
             Model.cursor
           ; collapsed =
               (if Set.mem model.collapsed path
                then Set.remove model.collapsed path
                else Set.add model.collapsed path)
           }
         | File _ -> { model with Model.cursor = cursor })
    | Expand ->
      (match List.nth_exn rows cursor with
       | Tree.Dir { path; expanded = false; _ } ->
         { model with Model.collapsed = Set.remove model.collapsed path }
       | Dir _ | File _ -> model)
    | Collapse { height } ->
      follow
        ~height
        (match List.nth_exn rows cursor with
         | Tree.Dir { path; expanded = true; _ } ->
           { model with Model.collapsed = Set.add model.collapsed path }
         | Dir _ | File _ ->
           (match parent rows ~cursor with
            | Some i -> { model with Model.cursor = i }
            | None -> model)))
;;

(* The file under the cursor, if any — a directory row selects nothing. *)
let selection rows ~cursor =
  match List.nth rows cursor with
  | Some (Tree.File { entry; _ }) -> Some entry.Git.Status.Entry.path
  | Some (Dir _) | None -> None
;;
