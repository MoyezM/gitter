open! Core
open Async

(** DESTRUCTIVE: reverts tracked changes and deletes untracked files under
    [path] (file or directory). Not recoverable through git — every caller
    must gate this behind an explicit user confirmation. *)
val discard_path : string -> unit Or_error.t Deferred.t
