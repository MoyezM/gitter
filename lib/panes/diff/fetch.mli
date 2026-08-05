open! Core
open! Bonsai_term

(** The async edge of the diff pane: the two-phase load protocol.

    Phase one stores the parsed diff (document built off the UI thread);
    phase two swaps in the highlight sessions when their domain parses
    finish. Superseded fetches — any later [load] or [clear] on the same
    [t] — never write and stop early. *)

type payload =
  { document : Document.t
  ; files : Git.Diff.File.t list
      (** the parsed diff, kept for hunk staging (verbatim hunk bytes) *)
  ; binary_only : bool
      (** the diff had files but no textual hunks (binary or mode-only) *)
  ; old_hl : Highlight.t (** HEAD-side session; [Highlight.empty] in phase one *)
  ; new_hl : Highlight.t (** worktree-side session; [Highlight.empty] in phase one *)
  }

(** What to fetch: the file plus WHICH change of it, VSCode-style — the
    Changes pane shows worktree vs index (relative to what's staged), the
    Staged pane shows index vs HEAD, the Committed pane shows merge-base
    vs HEAD against the carried base branch (base rides the key, so a
    base switch is a key change and resets the pane). *)
type side =
  [ `Staged
  | `Unstaged
  | `Committed of string (** the base branch *)
  ]
[@@deriving equal]

type key = string * side [@@deriving equal]

(** The stored result, tagged with the key it was fetched for. Consumers
    must check the tag against the current selection ([Render.content]
    does). *)
type result = (key * payload Or_error.t) option

type t

val create : unit -> t

(** Start the load for [key], writing phases through [set]. *)
val load : t -> key:key -> set:(result -> unit Effect.t) -> unit Effect.t

(** Selection cleared: invalidate any in-flight fetch and store None. *)
val clear : t -> set:(result -> unit Effect.t) -> unit Effect.t
