open! Core

(** Cursor, fold, and viewport state over the branch stack, pure — see
    state.ml for the fold defaults (top-level branches collapse unless
    they carry the current branch's chain) and the shared viewport
    contract. The search lifecycle lives in [Search.Tree_search]; this
    module supplies its pane record (branch names are the match
    candidates; groups carry ancestors; revealing unfolds via
    overrides). *)

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
  (** fold = the per-key collapse overrides (row key -> collapsed). *)
  type t = bool String.Map.t Search.Tree_search.Model.t

  val initial : t
end

module Action : sig
  (** Navigation is [Tree_listing.Action]; only the stack-specific key
      lives here. *)
  type t =
    | Nav of Tree_listing.Action.t
    | Enter of { height : int }
        (** set-base on a branch row (host wrapper schedules it at apply
            time); fold-toggle on a group row *)
  [@@deriving sexp_of]
end

(** The visible rows under the fold state alone (no search). *)
val visible
  :  branches:Git.Branch_stack.Branch.t list
  -> overrides:bool String.Map.t
  -> Row.t list

(** [Search.Tree_search.displayed_rows] over this pane, on the two
    model fields it reads (phys-stable host projections). *)
val displayed_rows
  :  branches:Git.Branch_stack.Branch.t list
  -> overrides:bool String.Map.t
  -> search:Search.Prompt.t
  -> Row.t list

val visible_rows : branches:Git.Branch_stack.Branch.t list -> Model.t -> Row.t list

(** (matching branch rows, total rows) — the border's counter. *)
val match_counts : branches:Git.Branch_stack.Branch.t list -> Search.Prompt.t -> int * int

(** The pane's bottom-border search line (prompt/register + counter). *)
val border
  :  branches:Git.Branch_stack.Branch.t list
  -> Search.Prompt.t
  -> width:int
  -> Bonsai_term.View.t option

val selection_key : Model.t -> string option
val scroll : Model.t -> int

(** The selection's position in [rows]; missing/unset keys read as the
    first row. *)
val index_of : Row.t list -> string option -> int

(** Pure transition over the WRAPPED action type ([Search.Action] owns
    mode dispatch and the search lifecycle). Total on empty branch
    lists. *)
val apply_action
  :  branches:Git.Branch_stack.Branch.t list
  -> Model.t
  -> Action.t Search.Action.t
  -> Model.t
