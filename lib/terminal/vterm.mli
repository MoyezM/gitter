open! Core
open! Bonsai_term

(** libvterm (the emulator neovim embeds) behind a small safe wrapper. The
    library owns all terminal semantics — parsing, grid state, and the
    INPUT ENCODING: keys and mouse are encoded by libvterm itself, honoring
    whatever modes the embedded application negotiated, so there is no mode
    tracking on the OCaml side. Encoded input and query responses come out
    of [take_output]; write them to the pty. *)

type t

val create : rows:int -> cols:int -> t

(** Feed pty bytes. Follow with [take_damage]/[take_output]. *)
val feed : t -> Bytes.t -> len:int -> unit

(** Drain pending output bytes (query responses, encoded input). *)
val take_output : t -> string

(** True once since the last call if the visible screen changed. *)
val take_damage : t -> bool

val resize : t -> rows:int -> cols:int -> unit

(** row, col, visible *)
val cursor : t -> int * int * bool

val send_key : t -> Event.Key.t -> Event.Modifier.t list -> unit

(** [row]/[col] are 0-based pane-local cells. libvterm encodes only if the
    app enabled a mouse mode — always safe to forward; check [take_output]
    emptiness to detect fallthrough (e.g. translate wheel to arrows). *)
val send_mouse
  :  t
  -> Event.mouse_kind
  -> row:int
  -> col:int
  -> mods:Event.Modifier.t list
  -> unit

(** The grid as a View (same-style runs merged; cursor as a reversed
    cell). *)
val render : t -> View.t

(** Test helpers. *)
val feed_string : t -> string -> unit

val row_text : t -> int -> string

val cell_style
  :  t
  -> r:int
  -> c:int
  -> [ `Attrs of int ]
     * [ `Fg of [ `Default | `Indexed of int | `Rgb of int * int * int ] ]
     * [ `Bg of [ `Default | `Indexed of int | `Rgb of int * int * int ] ]
