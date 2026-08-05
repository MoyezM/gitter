open! Core
open Async

(** The git queries the UI asks. Large outputs are parsed off the scheduler
    (a 500K-line diff costs ~100ms — several frames). *)

val status : unit -> Status.Entry.t list Or_error.t Deferred.t

(** The file's content at HEAD — the old side of an uncommitted diff, used
    for old-side syntax highlighting. Errors for untracked/new files. *)
val file_at_head : string -> string Or_error.t Deferred.t

(** The file's combined (staged + unstaged) change vs HEAD. Untracked files
    render as whole-file-added; a tracked file with no changes (or a
    directory) yields []. Paths are passed as literal pathspecs — glob
    characters in filenames are not special. *)
val diff_file_vs_head : string -> Diff.File.t list Or_error.t Deferred.t
