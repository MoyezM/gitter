open! Core

(** The branch stack, derived from git alone — no Graphite metadata. See
    stack.ml for the inference: each branch's parent is the branch with
    the deepest merge-base into its ancestry (computed inside the DAG
    above the branches' common base, with recent reflog tips standing in
    for amended parents), which also yields "needs restack" when the
    parent's current head has left the branch's ancestry. Pure and
    total. *)

module Branch : sig
  type t =
    { name : string
    ; depth : int (** 0 = trunk *)
    ; is_current : bool
    ; is_trunk : bool
    ; needs_restack : bool
    }
  [@@deriving sexp_of, equal]
end

(** [heads] is for-each-ref "<branch>\t<sha>" lines IN RECENCY ORDER (the
    caller sorts by -committerdate; line order ranks siblings); [dag] is
    rev-list --parents "<sha> <parent>..." lines for the commits above
    the branches' common base; [reflogs] is "<branch>\t<sha>" lines of
    recent reflog tips; [trunk] the trunk branch name; [current] the
    checked-out branch. Returns display order: depth-first from the trunk
    (the tree grows down, like the files tree); siblings order as the
    current branch's subtree first, then by subtree recency (a child's
    activity bubbles up to its parent), then name. Empty when [trunk] has
    no head. Fully-merged branches (tip strictly below the trunk head)
    are omitted unless current. *)
val parse
  :  heads:string
  -> dag:string
  -> reflogs:string
  -> trunk:string
  -> current:string option
  -> Branch.t list

(** The current branch's parent branch in a parsed stack — the default
    base for the committed-vs-base view. None when absent or on trunk. *)
val parent_of_current : Branch.t list -> string option

(** [name]'s parent branch — the base a review of [name] diffs against.
    None when [name] is absent or the trunk. *)
val parent_of : Branch.t list -> string -> string option
