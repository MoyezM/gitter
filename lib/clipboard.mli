open! Core
open! Async

(** Cross-platform copy-to-clipboard — see clipboard.ml for the strategy. *)

(** Copy [text] via the first usable native tool (pbcopy, wl-copy, xclip,
    xsel). Error when no tool is available or the spawn fails — fall back
    to writing [osc52 text] to the tty. *)
val copy_via_tool : string -> unit Or_error.t Deferred.t

(** The OSC 52 escape asking the hosting terminal to set the clipboard. *)
val osc52 : string -> string
