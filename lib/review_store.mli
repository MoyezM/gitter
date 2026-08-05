open! Core
open! Async

(** The on-disk review-mark store — SQLite at
    [<git common dir>/gitter/gitter.db], shared across worktrees. Marks
    are (old blob, new blob) content pairs; see review_store.ml. *)

(** All stored pairs (the table is created on first use). *)
val load : unit -> (string * string) list Or_error.t Deferred.t

(** Mark or unmark [pairs]. *)
val set : pairs:(string * string) list -> reviewed:bool -> unit Or_error.t Deferred.t
