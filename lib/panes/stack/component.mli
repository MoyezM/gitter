open! Core
open! Bonsai_term

(** The branch-stack pane: the inferred stack as a foldable tree.
    j/k/arrows move the cursor; h/l (or left/right) fold and unfold; click
    activates (toggles branches with children); the wheel scrolls the
    viewport without moving the cursor; ENTER (and only Enter — a
    misclick must not swap the diff base) runs [set_base] on the selected
    branch. [base] marks the committed view's current base row. Top-level
    branches off the current branch's chain start collapsed. *)

val component
  :  status:Render.status Bonsai.t
  -> base:string option Bonsai.t
  -> set_base:(string -> unit Effect.t) Bonsai.t
  -> Widget.t
