open! Core
open! Bonsai_term

(** One section's file tree (Staged or Changes — two instances, each its
    own layout pane). j/k/arrows move; left (or h) collapses the directory
    under the cursor or jumps to the parent; right (or l) expands; click
    activates (toggles directories, selects files). The wheel scrolls the
    viewport WITHOUT moving the cursor — the selection may sit off-screen;
    any cursor-moving key snaps the view back to it.

    The effectful keys (s/u/d/y/r) inject [Action.Operate] — the host's
    state machine resolves the target row at APPLY time and schedules the
    matching op from its input, so same-frame event bursts stay correct;
    c runs [commit] directly (no target). *)

val component
  :  status:Render.status Bonsai.t
  -> rows:Tree.row list Bonsai.t
  -> cursor:int Bonsai.t
  -> scroll:int Bonsai.t
  -> counts:(int * int) String.Map.t Bonsai.t
       (** per-file (added, removed) for this side; dirs sum their subtree *)
  -> reviewed:String.Set.t Bonsai.t
       (** row keys shown checked; [r] runs [toggle_review] on the row *)
  -> side:[ `Staged | `Unstaged | `Committed ]
  -> commit:unit Effect.t Bonsai.t
  -> inject:(State.Action.t -> unit Effect.t) Bonsai.t
  -> Widget.t
