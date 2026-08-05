open! Core
open Async

(** [git commit -m message] — commits what is staged. Errors (nothing
    staged, hooks) surface as ordinary [Error]s; callers resync. *)
val run : message:string -> unit Or_error.t Deferred.t
