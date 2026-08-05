open! Core
open! Async
open! Bonsai_term

(** A live embedded-terminal session: a command on a forkpty'd pty wired to
    a libvterm emulator. Generic — the editor overlay hosts one today; any
    pane wanting an embedded TUI can host one.

    Input is first-class: keys AND mouse are encoded by libvterm according
    to the modes the application negotiated (wheel degrades to arrow keys
    for mouse-less apps). Screens are tear-free grid generations —
    [publish] fires only when the visible screen actually changed. *)

type t

(** Spawns [command] under [/bin/sh -c] on a fresh pty in [cwd] with
    TERM=xterm-256color. [publish] fires from the Async scheduler on visible
    changes; [on_closed] once at pty EOF. *)
val create
  :  command:string
  -> cwd:string
  -> rows:int
  -> cols:int
  -> publish:(unit -> unit)
  -> on_closed:(unit -> unit)
  -> t

val is_closed : t -> bool

(** Synchronous encode + pty write. [Mouse] positions must be pane-local. *)
val send_event : t -> Event.t -> unit

val resize : t -> rows:int -> cols:int -> unit

(** SIGHUPs the child; the session marks itself closed (and releases the
    pty fd) when the reader sees EOF. *)
val close : t -> unit
val render : t -> View.t
val vterm : t -> Vterm.t
