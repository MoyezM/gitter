open! Core

(* Layout's own state: focus, per-split size overrides, zoom, and the
   in-progress divider drag. Nothing else in the app can touch it. *)

module Model = struct
  type t =
    { focused : string
    ; zoomed : bool
    ; fractions : float String.Map.t (* per-split overrides, by path key *)
    ; dragging : string option (* the dragged split's path key *)
    ; hidden : String.Set.t (* leaf ids pruned from the solve; state survives *)
    }
  [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | Focus of string
    | Focus_next
    | Toggle_zoom
    | Toggle_visible of string
    | Set_hidden of String.Set.t (* visibility preset; keeps focus valid *)
    | Show of string (* idempotent reveal (revealing can't orphan focus) *)
    | Start_drag of string
    | Set_frac of string * float
    | End_drag
  [@@deriving sexp_of]
end

(* The next visible leaf after [from], cycling. Total: falls back to [from]
   when nothing else is visible. *)
let next_visible ~leaf_ids ~hidden ~from =
  let visible = List.filter leaf_ids ~f:(fun id -> not (Set.mem hidden id)) in
  match visible with
  | [] -> from
  | _ ->
    let ix =
      List.findi visible ~f:(fun _ id -> String.equal id from)
      |> Option.value_map ~default:(-1) ~f:fst
    in
    List.nth_exn visible ((ix + 1) % List.length visible)
;;

let apply_action ~leaf_ids _ctx (model : Model.t) (action : Action.t) : Model.t =
  match action with
  | Focus id -> { model with focused = id }
  | Focus_next ->
    { model with focused = next_visible ~leaf_ids ~hidden:model.hidden ~from:model.focused }
  | Toggle_zoom -> { model with zoomed = not model.zoomed }
  | Toggle_visible id ->
    if Set.mem model.hidden id
    then { model with hidden = Set.remove model.hidden id }
    else (
      let hidden = Set.add model.hidden id in
      (* Never hide the last visible leaf (the solver needs at least one),
         and never leave focus on a hidden leaf (its handler would keep
         receiving keys for an invisible pane). *)
      if List.for_all leaf_ids ~f:(Set.mem hidden)
      then model
      else if String.equal model.focused id
      then { model with hidden; focused = next_visible ~leaf_ids ~hidden ~from:id }
      else { model with hidden })
  | Show id -> { model with hidden = Set.remove model.hidden id }
  | Set_hidden hidden ->
    if List.for_all leaf_ids ~f:(Set.mem hidden)
    then model (* never hide everything *)
    else if Set.mem hidden model.focused
    then { model with hidden; focused = next_visible ~leaf_ids ~hidden ~from:model.focused }
    else { model with hidden }
  | Start_drag path -> { model with dragging = Some path }
  | Set_frac (path, frac) ->
    { model with fractions = Map.set model.fractions ~key:path ~data:frac }
  | End_drag -> { model with dragging = None }
;;

let initial ?(hidden = String.Set.empty) ~first_leaf () =
  { Model.focused = first_leaf
  ; zoomed = false
  ; fractions = String.Map.empty
  ; dragging = None
  ; hidden
  }
;;
