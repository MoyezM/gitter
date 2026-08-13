open! Core
open! Bonsai_term

(** Pure View production for the branch-stack pane. *)

type status =
  [ `Loading
  | `Error of Error.t
  | `Empty of string
  | `Stack of Git.Branch_stack.Branch.t list
  ]

val render
  :  status:status
  -> rows:State.Row.t list (** the host's shared visible-rows derivation *)
  -> model:State.Model.t
  -> base:string option (** marked "(base)" on its row *)
  -> dimensions:Dimensions.t
  -> View.t
