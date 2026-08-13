open! Core
open! Bonsai_term

(** The modal combinator: confirmation and one-line input popups over the
    whole app. While open, the modal consumes every event (the base widget
    and the menu see nothing). State is hoisted so the root can hand the
    open-effects to panes. *)

val state
  :  local_ Bonsai.graph
  -> State.Model.t Bonsai.t * (State.Action.t -> unit Effect.t) Bonsai.t

module Controls : sig
  type t =
    { confirm : title:string -> body:string -> on_confirm:unit Effect.t -> unit Effect.t
        (** y/Enter runs [on_confirm]; n/Escape cancels *)
    ; prompt : title:string -> on_submit:(string -> unit Effect.t) -> unit Effect.t
        (** Enter submits the typed text (non-empty); Escape cancels *)
    }
end

val controls : inject:(State.Action.t -> unit Effect.t) Bonsai.t -> Controls.t Bonsai.t

val component
  :  model:State.Model.t Bonsai.t
  -> inject:(State.Action.t -> unit Effect.t) Bonsai.t
  -> Widget.screen
  -> dimensions:Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> Widget.screen
