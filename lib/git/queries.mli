open! Core
open Async

(** The git queries the UI asks. Large outputs are parsed off the scheduler
    (a 500K-line diff costs ~100ms — several frames). Paths are passed as
    literal pathspecs — glob characters in filenames are not special. *)

(** (raw porcelain output, entries, branch info) — raw is the poller's
    change signature. *)
val status
  :  unit
  -> (string * Status.Entry.t list * Status.Branch.t option) Or_error.t Deferred.t

(** The file's content at HEAD — the base of a staged diff. Errors for
    files new since HEAD. *)
val file_at_head : string -> string Or_error.t Deferred.t

(** The file's content in the index (stage 0) — the base of an unstaged
    diff, the result side of a staged one. Errors for untracked files. *)
val file_in_index : string -> string Or_error.t Deferred.t

(** The unstaged change: worktree vs index. Untracked files render as
    whole-file-added; a tracked-but-unchanged file (or a directory)
    yields []. *)
val diff_unstaged : string -> Diff.File.t list Or_error.t Deferred.t

(** The staged change: index vs HEAD. *)
val diff_staged : string -> Diff.File.t list Or_error.t Deferred.t
