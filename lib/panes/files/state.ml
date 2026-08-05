open! Core

module Model = struct
  type t =
    { cursor : int
    ; collapsed : String.Set.t
    ; scroll : int option
      (* Some = the wheel scrolled the viewport away from the cursor;
         None = the viewport follows the cursor. Any cursor-moving action
         clears it, which is what snaps the view back to the selection. *)
    }

  let initial = { cursor = 0; collapsed = String.Set.empty; scroll = None }
end

module Action = struct
  type t =
    | Move of [ `Up | `Down ]
    | Activate of int (* click: move the cursor there; toggles directories *)
    | Collapse (* left: fold the dir under the cursor, or jump to its parent *)
    | Expand (* right: unfold the dir under the cursor *)
    | Wheel of
        { dir : int (* +1 down, -1 up *)
        ; height : int (* pane inner height at dispatch time *)
        }
  [@@deriving sexp_of]
end

let clamp rows c = Int.clamp_exn c ~min:0 ~max:(Int.max 0 (List.length rows - 1))

(* Same flat step as the diff pane: velocity comes from the terminal's
   event rate, so a fixed per-tick step already scrolls faster on faster
   flicks. *)
let wheel_step = 3

(* The viewport when it follows the cursor: pinned so the cursor sits on
   the last row once past the first page. *)
let follow_offset ~cursor ~height = Int.max 0 (cursor - height + 1)

(* One owner for the viewport mapping: render and the click handler must
   agree on it or clicks activate the wrong row. *)
let offset ~total ~cursor ~height scroll =
  match scroll with
  | None -> follow_offset ~cursor ~height
  | Some s -> Int.max 0 (Int.min s (total - Int.max 1 height))
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

(* The cursor/collapse transitions; every one of these represents the user
   acting on the selection, so apply_action clears [scroll] around them. *)
let step rows (model : Model.t) (action : Action.t) =
  let cursor = model.Model.cursor in
  match action with
  | Action.Wheel _ -> model (* handled in apply_action *)
  | Move `Up -> { model with Model.cursor = clamp rows (cursor - 1) }
  | Move `Down -> { model with Model.cursor = clamp rows (cursor + 1) }
  | Activate i ->
    let cursor = clamp rows i in
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
  | Collapse ->
    (match List.nth_exn rows cursor with
     | Tree.Dir { path; expanded = true; _ } ->
       { model with Model.collapsed = Set.add model.collapsed path }
     | Dir _ | File _ ->
       (match parent rows ~cursor with
        | Some i -> { model with Model.cursor = i }
        | None -> model))
;;

let apply_action ~entries (model : Model.t) (action : Action.t) =
  let rows = Tree.rows ~entries ~collapsed:model.collapsed in
  if List.is_empty rows
  then { model with Model.cursor = 0; scroll = None }
  else (
    let cursor = clamp rows model.cursor in
    let model = { model with Model.cursor = cursor } in
    match action with
    | Action.Wheel { dir; height } ->
      (* Scroll the viewport only — the selection stays put (possibly
         off-screen). *)
      let base = offset ~total:(List.length rows) ~cursor ~height model.scroll in
      let limit = Int.max 0 (List.length rows - Int.max 1 height) in
      { model with
        Model.scroll = Some (Int.clamp_exn (base + (wheel_step * dir)) ~min:0 ~max:limit)
      }
    | action -> { (step rows model action) with Model.scroll = None })
;;

(* The file under the cursor, if any — a directory row selects nothing. *)
let selection rows ~cursor =
  match List.nth rows cursor with
  | Some (Tree.File { entry; _ }) -> Some entry.Git.Status.Entry.path
  | Some (Dir _) | None -> None
;;
