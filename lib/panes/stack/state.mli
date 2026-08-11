open! Core

(** Cursor, fold, and viewport state over the branch stack, pure — see
    state.ml for the fold defaults (top-level branches collapse unless
    they carry the current branch's chain) and the shared viewport
    contract (wheel scrolls without moving the cursor; cursor motion
    reveals with minimal scroll; clicking a visible row never moves the
    view). *)

module Row : sig
  type kind =
    | Branch of Git.Branch_stack.Branch.t
    | Group of { prefix : string }
      (** synthesized folder for sibling branches sharing a slash prefix *)

  type t =
    { kind : kind
    ; key : string (** fold-override key *)
    ; depth : int (** display depth (groups add a level) *)
    ; has_children : bool
    ; collapsed : bool (** only meaningful when has_children *)
    ; hidden : int (** branches hidden under a collapsed row *)
    ; on_current_stack : bool (** on the current branch's chain *)
    }
end

module Model : sig
  type t =
    { listing : Listing.Model.t
    ; overrides : bool String.Map.t (** row key -> collapsed, user-toggled *)
    }

  val initial : t
end

module Op : sig
  type t =
    | Set_base (* Enter *)
    | Review of { prefer_origin : bool } (* r: origin-preferred; R: local *)
  [@@deriving sexp_of]
end

module Action : sig
  type t =
    | Move of
        { dir : [ `Up | `Down ]
        ; height : int
        }
    | Activate of
        { row : int (** absolute visible-row index *)
        ; height : int
        }
    | Fold of { height : int } (** h: collapse, or jump to the parent *)
    | Unfold of { height : int } (** l *)
    | Wheel of
        { dir : int
        ; height : int
        }
    | Rows_changed (** the derived rows changed: repair the selection *)
    | Operate of
        { op : Op.t
        ; height : int
        }
        (** an effectful verb on the selected branch (host wrapper
            schedules it at apply time — burst-safe); fold-toggle on a
            group row *)
  [@@deriving sexp_of]
end

(** The visible rows under the fold state — the single row model shared by
    render, the click handler, and the transitions. *)
val visible
  :  branches:Git.Branch_stack.Branch.t list
  -> overrides:bool String.Map.t
  -> Row.t list

val selection_key : Model.t -> string option
val scroll : Model.t -> int

(** The selection's position in [rows]; missing/unset keys read as the
    first row. *)
val index_of : Row.t list -> string option -> int

(** Pure transition; total on empty branch lists. *)
val apply_action
  :  branches:Git.Branch_stack.Branch.t list
  -> Model.t
  -> Action.t
  -> Model.t
