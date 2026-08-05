open! Core
open! Bonsai_term

(** Pure View production for the branch-stack pane. *)

type status =
  [ `Loading
  | `Error of Error.t
  | `Empty of string
  | `Stack of Git.Branch_stack.Branch.t list
  ]

val render : status:status -> model:State.Model.t -> dimensions:Dimensions.t -> View.t
