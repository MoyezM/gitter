open! Core
open! Bonsai_term

(** The shared navigation core of the tree panes (files, stack): one
    action vocabulary, one transition, and one event shell over
    [Search.Tree_search.Model]. A pane embeds [Action.t] into its own
    action type (its extras — ops, Enter — stay its own), supplies
    [caps], and delegates. *)

module Action : sig
  (** Cursor-moving actions carry the pane height so the transition can
      reveal the selection. *)
  type t =
    | Move of
        { dir : [ `Up | `Down ]
        ; height : int
        }
    | Activate of
        { row : int (** absolute displayed-row index; click: toggles folds *)
        ; height : int
        }
    | Fold of { height : int } (** h/left: fold, or jump to the parent *)
    | Unfold of { height : int } (** l/right *)
    | Wheel of
        { dir : int (** +1 down, -1 up *)
        ; height : int
        }
    | Rows_changed (** the derived rows changed: repair the selection *)
  [@@deriving sexp_of]
end

(** The pane's search record plus what folding means for it. *)
type ('row, 'fold) caps =
  { pane : ('row, 'fold) Search.Tree_search.pane
  ; fold_state : 'row -> [ `Open | `Closed | `Leaf ]
  ; set_folded : 'fold -> key:string -> folded:bool -> 'fold
  }

(** The one tree-nav transition, over already-RESOLVED actions —
    [Search.Tree_search.apply] owns the mode dispatch and search
    lifecycle and delegates the pane's [Nav] arm here. *)
val apply
  :  ('row, 'fold) caps
  -> 'fold Search.Tree_search.Model.t
  -> Action.t
  -> 'fold Search.Tree_search.Model.t

(** The nav reading of printables — the fallback arm of the pane's
    [idle_char]; [lift] embeds [Action.t] into the pane's action type. *)
val idle_char
  :  height:int
  -> lift:(Action.t -> 'pane)
  -> char
  -> 'pane Search.Action.t option

(** Every non-keymap event: arrows, wheel, click-activate-then-commit. *)
val handle
  :  height:int
  -> total:int
  -> scroll:int
  -> inject:('pane Search.Action.t -> unit Effect.t)
  -> lift:(Action.t -> 'pane)
  -> Event.t
  -> unit Effect.t
