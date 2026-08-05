open! Core
open Async

(** The single process edge of the git layer; everything else is pure
    parsing. [accept_nonzero_exit] lists exit codes to treat as success
    (e.g. [1] for --no-index diffs). Errors carry the argv. *)
val git : ?accept_nonzero_exit:int list -> string list -> string Or_error.t Deferred.t
