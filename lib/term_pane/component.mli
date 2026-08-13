open! Core
open! Bonsai_term

(** The shell overlay: a padded modal terminal running [$SHELL] in the
    repo root, for quick git/gt commands. Ctrl-T toggles both ways (the
    shell keeps running while hidden); exiting the shell closes the
    overlay. Hiding or shell exit fires [on_hide] — the app passes an
    immediate git refresh so mutations show up without waiting for the
    poller. While visible, Ctrl-C is CAPTURED for the shell (SIGINT) and
    [on_ctrl_c] fires — the app posts a "Ctrl-T to leave" hint — so
    quit-gitter never triggers from inside the terminal. *)

type t

val create
  :  dimensions:Dimensions.t Bonsai.t
  -> on_show:unit Effect.t Bonsai.t
       (** fired as the overlay takes the screen — the app implicitly
           commits any active search prompt (L3) *)
  -> on_hide:unit Effect.t Bonsai.t
  -> on_ctrl_c:unit Effect.t Bonsai.t
  -> local_ Bonsai.graph
  -> t

module Controls : sig
  type t = { toggle : unit Effect.t }
end

val controls : t -> Controls.t Bonsai.t

(** Wrap the screen: overlays the terminal and consumes events while
    visible; Ctrl-T is intercepted in both states. *)
val wrap : t -> Widget.screen -> Widget.screen
