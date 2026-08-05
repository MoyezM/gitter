open! Core

(** Cursor and collapse state over ONE file tree, pure. Each section pane
    (Staged / Changes) is an independent instance, hosted by [Git_data]'s
    state machines — the root owns them because the derived selection feeds
    the diff pane.

    Behavioral contract: the cursor moves over VISIBLE rows; activating a
    directory toggles it; Collapse folds the directory under the cursor or,
    on a file/folded row, jumps to its enclosing directory; Expand unfolds. *)

module Model : sig
  type t =
    { cursor : int (** index into the current visible rows *)
    ; collapsed : String.Set.t (** full directory paths *)
    }

  val initial : t
end

module Action : sig
  type t =
    | Move of [ `Up | `Down ]
    | Activate of int (** click on a visible row *)
    | Collapse (** left *)
    | Expand (** right *)
  [@@deriving sexp_of]
end

(** Pure transition; recomputes the visible rows from [entries] (this
    pane's section only) and the model's collapsed set. Total on empty
    entry lists. *)
val apply_action : entries:Git.Status.Entry.t list -> Model.t -> Action.t -> Model.t

(** The file under the cursor; None on directory rows. *)
val selection : Tree.row list -> cursor:int -> string option
