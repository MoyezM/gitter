open! Core

(** Selection, collapse, viewport and search state over ONE file tree,
    pure. The selection/viewport laws live in [Listing]; the search
    lifecycle in [Search.Tree_search] (this module supplies its pane
    record — every row matches on its full path, directories carry
    descendants, revealing expands the ancestor chain). Hosted by
    [Git_data] (the root owns it because the derived selection feeds
    the diff pane), which injects [Rows_changed] whenever the derived
    rows' keys change. *)

module Model : sig
  (** fold = the collapsed-directory set (full paths). *)
  type t = String.Set.t Search.Tree_search.Model.t

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
  (** Navigation is [Tree_listing.Action]; only the files-specific
      effectful keys live here. *)
  type t =
    | Nav of Tree_listing.Action.t
    | Operate of Op.t (** handled by the host's apply_action wrapper *)
    | Commit_prompt (** [c]: targetless, so not an [Op]; host schedules it *)
  [@@deriving sexp_of]
end

(** The row's selection key: a file's path, a directory's full path. *)
val row_key : Tree.row -> string

val selection_key : Model.t -> string option
val scroll : Model.t -> int

(** The selection's position in [rows]; missing/unset keys read as the
    first row. *)
val index_of : Tree.row list -> string option -> int

(** [Search.Tree_search.displayed_rows] over this pane, on the two
    model fields it reads (phys-stable host projections). *)
val displayed_rows
  :  entries:Git.Status.Entry.t list
  -> collapsed:String.Set.t
  -> search:Search.Prompt.t
  -> Tree.row list

val visible_rows : entries:Git.Status.Entry.t list -> Model.t -> Tree.row list

(** (matching rows, total rows) — the border's counter. *)
val match_counts : entries:Git.Status.Entry.t list -> Search.Prompt.t -> int * int

(** Whether [n]/[N] would actually jump — a register with a live match. *)
val can_jump : entries:Git.Status.Entry.t list -> Search.Prompt.t -> bool

(** Pure transition over the WRAPPED action type ([Search.Action] owns
    mode dispatch and the search lifecycle; this pane's own actions
    delegate through). Total on empty entry lists. *)
val apply_action
  :  entries:Git.Status.Entry.t list
  -> Model.t
  -> Action.t Search.Action.t
  -> Model.t

(** The file at [cursor]; None on directory rows. *)
val selection : Tree.row list -> cursor:int -> string option

(** The file feeding the diff pane: the selection, with the T5 fallback —
    when an active search narrows [rows] to nothing, the pre-prompt
    selection stays effective, resolved against the unfiltered tree.
    Takes model PIECES so hosts feed phys-stable projections
    ([pre_prompt] = [Search.Tree_search.pre_prompt_selection]). *)
val effective_selection
  :  entries:Git.Status.Entry.t list
  -> rows:Tree.row list
  -> search:Search.Prompt.t
  -> fold:String.Set.t
  -> pre_prompt:string option
  -> selection:string option
  -> string option

(** The operation target under the CURRENT selection (file path or dir
    subtree path) — what the host's [Operate] wrapper acts on. *)
val target : entries:Git.Status.Entry.t list -> Model.t -> string option
