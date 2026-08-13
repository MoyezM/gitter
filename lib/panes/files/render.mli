open! Core
open! Bonsai_term

(** Pure View production for one file-tree pane. The viewport offset comes
    from [State.offset] — one owner, shared with the click handler. *)

(** The root maps its load state to this — the pane doesn't know Git_data.
    [`Loaded] carries this pane's idle message; whether the tree or the
    message shows is derived HERE from rows+search (T5: an active
    zero-match search renders an empty tree with its counter, never the
    idle message). [`Empty] is unconditional — no tree regardless (the
    committed pane with no base). *)
type status =
  [ `Loading
  | `Error of Error.t
  | `Empty of string
  | `Loaded of string
  ]

val render
  :  status:status
  -> rows:Tree.row list
  -> cursor:int
  -> scroll:int (** the model's viewport offset (render clamps it) *)
  -> counts:(int * int) String.Map.t
       (** per-file (added, removed); dirs sum their subtree *)
  -> reviewed:String.Set.t
       (** row keys to show checked and dimmed (files + fully-reviewed dirs) *)
  -> side:[ `Staged | `Unstaged | `Committed ] (** which side's status letter to show *)
  -> search:Search.Prompt.t (** the live query underlines its matches (T3) *)
  -> dimensions:Dimensions.t
  -> View.t
