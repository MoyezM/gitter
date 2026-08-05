open! Core
open! Bonsai_term

(** A one-column vertical scrollbar for any windowed surface. *)

(** Thumb (row, length) within a track of [visible] cells — proportional to
    the visible fraction, pinned to the ends at the extremes. None when
    everything fits (draw no bar and keep the column). Exposed for tests. *)
val thumb : total:int -> visible:int -> offset:int -> (int * int) option

(** The bar as a [visible]-cell-tall, one-column View; None when content
    fits. *)
val view : total:int -> visible:int -> offset:int -> View.t option

(** Overlay the bar as [body]'s rightmost column ([width] is the pane's
    inner width); [body] unchanged when there is no bar. *)
val attach : width:int -> bar:View.t option -> View.t -> View.t
