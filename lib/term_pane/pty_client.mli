open! Core
open! Async
open! Bonsai_term

(** The pty side of the embedded terminal: spawns the command on a forkpty'd
    pty (C stub — the OCaml runtime never forks), feeds master-fd bytes into
    a [Vt], and writes encoded input back. No subprocesses after spawn, no
    snapshots — the Vt publishes coalesced, tear-free screen generations.

    Mouse is first-class: SGR-encoded into the pty when the application
    enabled a mouse mode; otherwise the wheel degrades to arrow keys. *)

type t

(** [publish] fires on every visible screen change (from the Async
    scheduler); [on_closed] once when the child's pty reaches EOF. *)
val create
  :  command:string
  -> cwd:string
  -> rows:int
  -> cols:int
  -> publish:(unit -> unit)
  -> on_closed:(unit -> unit)
  -> t

val vt : t -> Vt.t
val is_closed : t -> bool

(** Non-blocking. [Mouse] positions must already be pane-local. *)
val send_event : t -> Event.t -> unit

val resize : t -> rows:int -> cols:int -> unit
val close : t -> unit
