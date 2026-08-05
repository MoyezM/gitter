open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The modal combinator: wraps a widget; while a modal is open it draws the
   popup over the base and consumes every event. State is hoisted (like
   Layout's) so the root can hand [Controls] effects to any pane. Sits
   OUTSIDE the menu wrapper — Space must not open the menu over a modal. *)

let state (local_ graph) =
  Bonsai.state_machine
    ~default_model:State.Model.Closed
    ~apply_action:(fun _ctx model action -> State.apply_action model action)
    graph
;;

module Controls = struct
  type t =
    { confirm : title:string -> body:string -> on_confirm:unit Effect.t -> unit Effect.t
    ; prompt : title:string -> on_submit:(string -> unit Effect.t) -> unit Effect.t
    }
end

let controls ~inject =
  let%arr inject in
  { Controls.confirm =
      (fun ~title ~body ~on_confirm ->
        inject (State.Action.Open (State.Model.Confirm { title; body; on_confirm })))
  ; prompt =
      (fun ~title ~on_submit ->
        inject (State.Action.Open (State.Model.Input { title; text = ""; on_submit })))
  }
;;

let component ~model ~inject (base : Widget.t) : Widget.t =
  fun ~dimensions (local_ graph) ->
  let ~view:base_view, ~handler:base_handler = base ~dimensions graph in
  let peek_model = Bonsai.peek model graph in
  let view =
    let%arr base_view and model and dimensions in
    match (model : State.Model.t) with
    | Closed -> base_view
    | open_modal -> View.zcat [ Render.render open_modal ~dimensions; base_view ]
  in
  let handler =
    let%arr base_handler and model and inject and peek_model in
    (* Confirm/submit read the model AT EFFECT TIME (peek): the typed text
       must be the accumulated current state, not this frame's snapshot. *)
    let finish =
      let%bind.Effect peeked = peek_model in
      match peeked with
      | Bonsai.Computation_status.Active (State.Model.Confirm { on_confirm; _ }) ->
        Effect.Many [ inject State.Action.Close; on_confirm ]
      | Active (Input { text; on_submit; _ }) ->
        if String.is_empty (String.strip text)
        then Effect.Ignore
        else Effect.Many [ inject State.Action.Close; on_submit text ]
      | Active Closed | Inactive -> Effect.Ignore
    in
    fun (event : Event.t) ->
      match (model : State.Model.t) with
      | Closed -> base_handler event
      | Confirm _ ->
        (match event with
         | Event.Key_press { key = ASCII ('y' | 'Y'); mods = [] }
         | Key_press { key = Enter; mods = [] } -> finish
         | Key_press { key = ASCII ('n' | 'N'); mods = [] }
         | Key_press { key = Escape; _ } -> inject State.Action.Close
         | Key_press _ | Mouse _ | Paste _ -> Effect.Ignore)
      | Input _ ->
        (match event with
         | Event.Key_press { key = Escape; _ } -> inject State.Action.Close
         | Key_press { key = Enter; mods = [] } -> finish
         | Key_press { key = Backspace; _ } -> inject State.Action.Backspace
         | Key_press { key = ASCII c; mods = [] } -> inject (State.Action.Append c)
         | Key_press _ | Mouse _ | Paste _ -> Effect.Ignore)
  in
  ~view, ~handler
;;
