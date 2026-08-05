open! Core

(** Selection, collapse, and viewport state over ONE file tree, pure. The
    selection/viewport laws live in [Listing]; this module owns the
    collapse set and dir/file activation semantics. Hosted by [Git_data]
    (the root owns it because the derived selection feeds the diff pane),
    which injects [Rows_changed] whenever the derived rows' keys
    change. *)

module Model : sig
  type t =
    { listing : Listing.Model.t
    ; collapsed : String.Set.t (** full directory paths *)
    }

  val initial : t
end

module Op : sig
  (** The effectful keys, routed through the machine so targets resolve at
      apply time (burst-safe) — the host's apply_action wrapper schedules
      the effect. *)
  type t =
    | Stage
    | Unstage
    | Discard
    | Copy_path
    | Toggle_review
  [@@deriving sexp_of]
end

module Action : sig
  (** Cursor-moving actions carry the pane height so the transition can
      reveal the selection. *)
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
    | Rows_changed (** the derived rows changed: repair the selection *)
    | Operate of Op.t (** handled by the host's apply_action wrapper *)
  [@@deriving sexp_of]
end

(** The row's selection key: a file's path, a directory's full path. *)
val row_key : Tree.row -> string

val selection_key : Model.t -> string option
val scroll : Model.t -> int

(** The selection's position in [rows]; missing/unset keys read as the
    first row. *)
val index_of : Tree.row list -> string option -> int

(** Pure transition; recomputes the visible rows from [entries] (this
    pane's section only) and the model's collapsed set. Total on empty
    entry lists. *)
val apply_action : entries:Git.Status.Entry.t list -> Model.t -> Action.t -> Model.t

(** The file at [cursor]; None on directory rows. *)
val selection : Tree.row list -> cursor:int -> string option

(** The operation target under the CURRENT selection (file path or dir
    subtree path) — what the host's [Operate] wrapper acts on. *)
val target : entries:Git.Status.Entry.t list -> Model.t -> string option
