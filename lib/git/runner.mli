open! Core
open Async

(** The single process edge of the git layer; everything else is pure
    parsing. [stdin] feeds the child (e.g. a patch for [git apply -]).
    [accept_nonzero_exit] lists exit codes to treat as success (e.g. [1]
    for --no-index diffs). Errors carry the argv. *)
val git
  :  ?stdin:string
  -> ?accept_nonzero_exit:int list
  -> string list
  -> string Or_error.t Deferred.t

(** Wrap a path for use after [--]: pathspecs treat glob characters
    specially; the literal magic makes them plain filenames. *)
val literal : string -> string
