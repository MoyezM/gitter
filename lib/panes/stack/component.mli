open! Core
open! Bonsai_term

(** The branch-stack pane: the inferred stack as a foldable tree.
    j/k/arrows move the cursor; h/l (or left/right) fold and unfold; click
    activates (toggles branches with children); the wheel scrolls the
    viewport without moving the cursor. Top-level branches off the
    current branch's chain start collapsed. Set-as-base arrives with
    relative mode. *)

val component : status:Render.status Bonsai.t -> Widget.t
