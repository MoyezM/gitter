open! Core

(** Cursor, collapse, and viewport state over ONE file tree, pure. Each
    section pane (Staged / Changes) is an independent instance, hosted by
    [Git_data]'s state machines — the root owns them because the derived
    selection feeds the diff pane.

    Behavioral contract: the cursor moves over VISIBLE rows; activating a
    directory toggles it; Collapse folds the directory under the cursor or,
    on a file/folded row, jumps to its enclosing directory; Expand unfolds.
    The WHEEL scrolls the viewport without moving the cursor — the
    selection may sit off-screen; any cursor-moving action clears the
    scroll override, snapping the view back to the selection. *)

module Model : sig
  type t =
    { cursor : int (** index into the current visible rows *)
    ; collapsed : String.Set.t (** full directory paths *)
    ; scroll : int option
      (** wheel-scrolled viewport offset; [None] follows the cursor *)
    }

  val initial : t
end

module Action : sig
  type t =
    | Move of [ `Up | `Down ]
    | Activate of int (** click on a visible row *)
    | Collapse (** left *)
    | Expand (** right *)
    | Wheel of
        { dir : int (** +1 down, -1 up *)
        ; height : int (** pane inner height at dispatch time *)
        }
  [@@deriving sexp_of]
end

(** Pure transition; recomputes the visible rows from [entries] (this
    pane's section only) and the model's collapsed set. Total on empty
    entry lists. *)
val apply_action : entries:Git.Status.Entry.t list -> Model.t -> Action.t -> Model.t

(** The single owner of the viewport mapping — render and the click
    handler must agree on it or clicks activate the wrong row. [scroll] is
    the model's override, clamped against [total] rows. *)
val offset : total:int -> cursor:int -> height:int -> int option -> int

(** The file under the cursor; None on directory rows. *)
val selection : Tree.row list -> cursor:int -> string option
