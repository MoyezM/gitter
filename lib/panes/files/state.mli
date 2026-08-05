open! Core

(** Cursor, collapse, and viewport state over ONE file tree, pure. Each
    section pane (Staged / Changes) is an independent instance, hosted by
    [Git_data]'s state machines — the root owns them because the derived
    selection feeds the diff pane.

    Behavioral contract: the cursor moves over VISIBLE rows; activating a
    directory toggles it; Collapse folds the directory under the cursor or,
    on a file/folded row, jumps to its enclosing directory; Expand unfolds.
    The WHEEL scrolls the viewport without moving the cursor — the
    selection may sit off-screen; cursor-moving actions reveal it again
    with minimal scroll motion (clicking a visible row never moves the
    view). *)

module Model : sig
  type t =
    { cursor : int (** index into the current visible rows *)
    ; collapsed : String.Set.t (** full directory paths *)
    ; scroll : int (** first visible row; the wheel moves it freely *)
    }

  val initial : t
end

module Action : sig
  (** Cursor-moving actions carry the pane height so the transition can
      reveal the cursor (the state machine lives in [Git_data], which has
      no dimensions — the pane handler does). *)
  type t =
    | Move of
        { dir : [ `Up | `Down ]
        ; height : int
        }
    | Activate of
        { row : int (** absolute row index; click: toggles directories *)
        ; height : int
        }
    | Collapse of { height : int } (** left *)
    | Expand (** right *)
    | Wheel of
        { dir : int (** +1 down, -1 up *)
        ; height : int
        }
  [@@deriving sexp_of]
end

(** Pure transition; recomputes the visible rows from [entries] (this
    pane's section only) and the model's collapsed set. Total on empty
    entry lists. *)
val apply_action : entries:Git.Status.Entry.t list -> Model.t -> Action.t -> Model.t

(** The single owner of the viewport mapping — render and the click
    handler must agree on it or clicks activate the wrong row: the model's
    scroll clamped against [total] rows and the pane [height]. *)
val offset : total:int -> height:int -> int -> int

(** The file under the cursor; None on directory rows. *)
val selection : Tree.row list -> cursor:int -> string option
