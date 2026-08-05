open! Core

module Model = struct
  type t =
    { cursor : int
    ; collapsed : String.Set.t
    }

  let initial = { cursor = 0; collapsed = String.Set.empty }
end

module Action = struct
  type t =
    | Move of [ `Up | `Down ]
    | Activate of int (* click: move the cursor there; toggles directories *)
    | Collapse (* left: fold the dir under the cursor, or jump to its parent *)
    | Expand (* right: unfold the dir under the cursor *)
  [@@deriving sexp_of]
end

let clamp rows c = Int.clamp_exn c ~min:0 ~max:(Int.max 0 (List.length rows - 1))

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
  then { model with Model.cursor = 0 }
  else (
    let cursor = clamp rows model.cursor in
    let model = { model with Model.cursor = cursor } in
    match action with
    | Action.Move `Up -> { model with Model.cursor = clamp rows (cursor - 1) }
    | Move `Down -> { model with Model.cursor = clamp rows (cursor + 1) }
    | Activate i ->
      let cursor = clamp rows i in
      (match List.nth_exn rows cursor with
       | Tree.Dir { path; _ } ->
         { Model.cursor
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
          | None -> model)))
;;

(* The file under the cursor, if any — a directory row selects nothing. *)
let selection rows ~cursor =
  match List.nth rows cursor with
  | Some (Tree.File { entry; _ }) -> Some entry.Git.Status.Entry.path
  | Some (Dir _) | None -> None
;;
