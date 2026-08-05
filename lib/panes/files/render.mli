open! Core
open! Bonsai_term

(** Pure View production for one file-tree pane. *)

(** The click handler must map screen rows through the same offset render
    uses. *)
val offset : cursor:int -> height:int -> int

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
  -> side:[ `Staged | `Unstaged ] (** which side's status letter to show *)
  -> dimensions:Dimensions.t
  -> View.t
