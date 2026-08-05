open! Core
open! Bonsai_term

(** Pure View production for one file-tree pane. The viewport offset comes
    from [State.offset] — one owner, shared with the click handler. *)

(** The root maps its load state to this — the pane doesn't know Git_data.
    [`Empty] carries this pane's idle message. *)
type status =
  [ `Loading
  | `Error of Error.t
  | `Empty of string
  | `Tree
  ]

val render
  :  status:status
  -> rows:Tree.row list
  -> cursor:int
  -> scroll:int (** the model's viewport offset (render clamps it) *)
  -> counts:(int * int) String.Map.t
       (** per-file (added, removed); dirs sum their subtree *)
  -> side:[ `Staged | `Unstaged ] (** which side's status letter to show *)
  -> dimensions:Dimensions.t
  -> View.t
