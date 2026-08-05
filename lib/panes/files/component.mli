open! Core
open! Bonsai_term

(** One section's file tree (Staged or Changes — two instances, each its
    own layout pane). j/k/arrows move; left (or h) collapses the directory
    under the cursor or jumps to the parent; right (or l) expands; click
    activates (toggles directories, selects files). The wheel scrolls the
    viewport WITHOUT moving the cursor — the selection may sit off-screen;
    any cursor-moving key snaps the view back to it.

    s/u/d run [stage]/[unstage]/[discard] on the row under the cursor
    (files and whole directories alike); c runs [commit]; y runs
    [copy_path] (copy the row's path to the clipboard). The root passes
    no-ops for operations that don't apply to a side (discard is
    Changes-only and MUST arrive pre-wrapped in a confirmation — it is
    worktree-destructive). *)

val component
  :  status:Render.status Bonsai.t
  -> rows:Tree.row list Bonsai.t
  -> cursor:int Bonsai.t
  -> scroll:int Bonsai.t
  -> counts:(int * int) String.Map.t Bonsai.t
       (** per-file (added, removed) for this side; dirs sum their subtree *)
  -> side:[ `Staged | `Unstaged | `Committed ]
  -> stage:(string -> unit Effect.t) Bonsai.t
  -> unstage:(string -> unit Effect.t) Bonsai.t
  -> discard:(string -> unit Effect.t) Bonsai.t
  -> commit:unit Effect.t Bonsai.t
  -> copy_path:(string -> unit Effect.t) Bonsai.t
  -> inject:(State.Action.t -> unit Effect.t) Bonsai.t
  -> Widget.t
