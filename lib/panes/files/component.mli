open! Core
open! Bonsai_term

(** One section's file tree (Staged or Changes — two instances, each its
    own layout pane). j/k/arrows move; left (or h) collapses the directory
    under the cursor or jumps to the parent; right (or l) expands; click
    activates (toggles directories, selects files). The wheel scrolls the
    viewport WITHOUT moving the cursor — the selection may sit off-screen;
    any cursor-moving key snaps the view back to it.

    The effectful keys (s/u/d/c/y/r) inject [Action.Operate] — the host's
    state machine resolves the target row at APPLY time and schedules the
    matching op from its input, so same-frame event bursts stay correct.
    Mode-dependent keys ride [Search.Action] (which owns the prompt
    keymap and mode dispatch): the handler never branches on a captured
    prompt state.

    [/] opens the search prompt (rendered into the pane's bottom border
    — the [bordered] half); while it is active every printable appends
    to the query and the tree narrows live. [n]/[N] jump among a
    committed register's matches; Esc clears it. *)

(** One section's assembled inputs — built in one place (the host) so a
    new pane output is a field here, not a parameter threaded through
    every layer. *)
module Input : sig
  type t =
    { status : Render.status Bonsai.t
    ; rows : Tree.row list Bonsai.t (** the VISIBLE rows — narrowed while searching *)
    ; cursor : int Bonsai.t
    ; scroll : int Bonsai.t
    ; counts : (int * int) String.Map.t Bonsai.t
      (** per-file (added, removed) for this side; dirs sum their subtree *)
    ; reviewed : String.Set.t Bonsai.t
      (** row keys shown checked; [r] runs [toggle_review] on the row *)
    ; side : [ `Staged | `Unstaged | `Committed ]
    ; search : Search.Prompt.t Bonsai.t
    ; search_counts : (int * int) Bonsai.t (** (matching rows, total rows) *)
    ; inject : (State.Action.t Search.Action.t -> unit Effect.t) Bonsai.t
    ; hints : string
      (** status-bar key hints — assembled by the host next to the ops
          wiring, the one place that knows which keys this section acts on *)
    }
end

val component : Input.t -> Widget.leaf
