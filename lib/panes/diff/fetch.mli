open! Core
open! Bonsai_term

(** The async edge of the diff pane: the two-phase load protocol.

    Phase one stores the parsed diff (document built off the UI thread);
    phase two swaps in the highlight sessions when their domain parses
    finish. Superseded fetches — any later [load] or [clear] on the same
    [t] — never write and stop early. *)

type payload =
  { document : Document.t
  ; binary_only : bool
      (** the diff had files but no textual hunks (binary or mode-only) *)
  ; old_hl : Highlight.t (** HEAD-side session; [Highlight.empty] in phase one *)
  ; new_hl : Highlight.t (** worktree-side session; [Highlight.empty] in phase one *)
  }

(** The stored result, tagged with the path it was fetched for. Consumers
    must check the tag against the current selection ([Render.content]
    does). *)
type result = (string * payload Or_error.t) option

type t

val create : unit -> t

(** Start the load for [path], writing phases through [set]. *)
val load : t -> path:string -> set:(result -> unit Effect.t) -> unit Effect.t

(** Selection cleared: invalidate any in-flight fetch and store None. *)
val clear : t -> set:(result -> unit Effect.t) -> unit Effect.t
