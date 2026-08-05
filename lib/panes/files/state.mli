open! Core

(** Selection, collapse, and viewport state over ONE file tree, pure. Each
    section pane (Staged / Changes) is an independent instance, hosted by
    [Git_data]'s state machines — the root owns them because the derived
    selection feeds the diff pane.

    Selection is a KEY (the row's path), not an index — see state.ml for
    the law: stable under reorder; on removal it moves to the nearest
    surviving successor (then predecessor) in the old traversal, which is
    also what makes staging flow to the next file. The host injects
    [Rows_changed] whenever the derived rows' keys change. The wheel
    scrolls the viewport without moving the selection. *)

module Model : sig
  type t =
    { selection : string option (** row key; [None] = nothing chosen yet *)
    ; collapsed : String.Set.t (** full directory paths *)
    ; scroll : int
    ; keys : string list (** rows' keys at the last transition *)
    }

  val initial : t
end

module Action : sig
  (** Cursor-moving actions carry the pane height so the transition can
      reveal the selection (the state machine lives in [Git_data], which
      has no dimensions — the pane handler does). *)
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
  [@@deriving sexp_of]
end

(** The row's selection key: a file's path, a directory's full path. *)
val row_key : Tree.row -> string

(** The single owner of the viewport mapping — render and the click
    handler must agree on it. *)
val offset : total:int -> height:int -> int -> int

(** The selection's position in [rows]; missing/unset keys read as the
    first row, so callers always have a valid cursor for rendering and
    operations. *)
val index_of : Tree.row list -> string option -> int

(** The repair law, exposed for tests: keep a surviving key; otherwise the
    first surviving successor in [old_keys], else the first surviving
    predecessor, else [None]. *)
val repair
  :  old_keys:string list
  -> selection:string option
  -> new_keys:string list
  -> string option

(** Pure transition; recomputes the visible rows from [entries] (this
    pane's section only) and the model's collapsed set. Total on empty
    entry lists. *)
val apply_action : entries:Git.Status.Entry.t list -> Model.t -> Action.t -> Model.t

(** The file at [cursor]; None on directory rows. *)
val selection : Tree.row list -> cursor:int -> string option
