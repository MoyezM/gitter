open! Core

(* Layout's own state: focus, per-split size overrides, zoom, and the
   in-progress divider drag. Nothing else in the app can touch it. *)

module Model = struct
  type t =
    { focused : string
    ; zoomed : bool
    ; fractions : float String.Map.t (* per-split overrides, by path key *)
    ; dragging : string option (* the dragged split's path key *)
    }
  [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | Focus of string
    | Focus_next
    | Toggle_zoom
    | Start_drag of string
    | Set_frac of string * float
    | End_drag
  [@@deriving sexp_of]
end

let apply_action ~leaf_ids _ctx (model : Model.t) (action : Action.t) : Model.t =
  match action with
  | Focus id -> { model with focused = id }
  | Focus_next ->
    let ix =
      List.findi leaf_ids ~f:(fun _ id -> String.equal id model.focused)
      |> Option.value_map ~default:0 ~f:fst
    in
    let focused = List.nth_exn leaf_ids ((ix + 1) % List.length leaf_ids) in
    { model with focused }
  | Toggle_zoom -> { model with zoomed = not model.zoomed }
  | Start_drag path -> { model with dragging = Some path }
  | Set_frac (path, frac) ->
    { model with fractions = Map.set model.fractions ~key:path ~data:frac }
  | End_drag -> { model with dragging = None }
;;

let initial ~first_leaf =
  { Model.focused = first_leaf
  ; zoomed = false
  ; fractions = String.Map.empty
  ; dragging = None
  }
;;
