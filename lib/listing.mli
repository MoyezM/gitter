open! Core

(** The shared core of every scrollable, key-selected listing — see
    listing.ml for the laws (key selection stable under reorder with
    successor repair; wheel scrolls without moving the selection;
    selection motion reveals minimally). *)

module Model : sig
  type t =
    { selection : string option (** row key; [None] = nothing chosen yet *)
    ; scroll : int
    ; keys : string list (** the rows' keys at the last transition *)
    }

  val initial : t
end

val wheel_step : int

(** The viewport mapping — render and click handlers must agree on it. *)
val offset : total:int -> height:int -> int -> int

(** The row under screen line [y], through the same viewport mapping the
    renderer uses; None below the last row (blank space is not a row). *)
val row_at : total:int -> height:int -> y:int -> int -> int option

(** Scroll so [cursor] is visible with minimal movement; [margin] keeps it
    away from the edges (the diff pane's follow), shrunk on tiny panes. *)
val reveal : ?margin:int -> total:int -> height:int -> cursor:int -> int -> int

(** The selection's position; missing/unset keys read as the first row. *)
val index_of : key:('r -> string) -> 'r list -> string option -> int

(** The nearest earlier row one depth shallower — the visible parent. *)
val parent_index : depth:('r -> int) -> 'r list -> int -> int option

(** The repair law, exposed for tests: keep a surviving key; else the
    first surviving successor in [old_keys], else predecessor, else
    [None]. *)
val repair
  :  old_keys:string list
  -> selection:string option
  -> new_keys:string list
  -> string option

(** The no-rows model: no selection, zero scroll, no keys. *)
val empty : Model.t

(** Scroll by [wheel_step * dir], selection untouched. *)
val wheel : Model.t -> total:int -> height:int -> dir:int -> Model.t

(** Select the row at [index] (clamped), snapshot keys; with [height],
    reveal it. *)
val select : key:('r -> string) -> 'r list -> ?height:int -> index:int -> Model.t -> Model.t

(** The rows changed underneath us: repair the selection, snapshot. *)
val rows_changed : key:('r -> string) -> 'r list -> Model.t -> Model.t
