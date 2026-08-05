open! Core
open! Bonsai_term

(** A VT/xterm terminal emulator sized for full-screen editors: a mutable
    cell grid fed by raw pty bytes. Snapshot approaches (tmux capture in any
    transport) sample mid-redraw — tearing, waterfalling, flashes — and
    can't do first-class mouse; feeding the byte stream statefully makes
    torn frames impossible: synchronized-output (DECSET 2026) batches
    publish atomically, everything else publishes between feed chunks.

    Single-threaded by design: feed, render, and reads all happen on the
    Async scheduler thread. *)

module Style : sig
  module Color : sig
    type t =
      | Default
      | Idx of int
      | Rgb of int * int * int
    [@@deriving equal, sexp_of]
  end

  type t =
    { fg : Color.t
    ; bg : Color.t
    ; bold : bool
    ; dim : bool
    ; italic : bool
    ; underline : bool
    ; reverse : bool
    }
  [@@deriving equal, sexp_of]

  val default : t
end

module Mouse_mode : sig
  type t =
    | Off
    | Normal (** 1000: press/release/wheel *)
    | Button (** 1002: + drag *)
    | Any (** 1003: + motion *)
  [@@deriving equal, sexp_of]
end

type t

(** [respond] carries answers to identification queries (DA, CPR, OSC
    color queries) — write them back to the pty. [publish] fires when the
    visible screen changed (coalesced per feed chunk; held during
    synchronized-output blocks). *)
val create
  :  rows:int
  -> cols:int
  -> respond:(string -> unit)
  -> publish:(unit -> unit)
  -> t

val feed : t -> Bytes.t -> len:int -> unit
val feed_string : t -> string -> unit
val resize : t -> rows:int -> cols:int -> unit

(** The grid as a View (same-style runs merged); the cursor renders as a
    reversed cell when visible. *)
val render : t -> View.t

(** For the input encoder: what the application asked of its terminal. *)
val mouse_mode : t -> Mouse_mode.t

val mouse_sgr : t -> bool
val app_cursor_keys : t -> bool

(** Monotonic screen generation — cheap change detection. *)
val seq : t -> int

(** Test introspection. *)
val cursor : t -> int * int

val cursor_visible : t -> bool
val row_text : t -> int -> string
val style_at : t -> r:int -> c:int -> Style.t
