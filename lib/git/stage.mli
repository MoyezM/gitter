open! Core
open Async

(** Index mutations — the only writes in the git layer, and all index-only:
    none can touch worktree content. Failures (e.g. the index moved since
    the diff was displayed) are ordinary [Error]s; callers resync. *)

(** [git add] — stages a file or everything under a directory. Handles
    modified, untracked, and deleted paths uniformly. *)
val stage_path : string -> unit Or_error.t Deferred.t

(** [git restore --staged] — the inverse, for a file or directory. *)
val unstage_path : string -> unit Or_error.t Deferred.t

(** The exact patch text an apply uses, exposed for tests. [raw] is
    [Diff.Hunk.raw] — the parser's verbatim hunk bytes. *)
val patch : path:string -> raw:string -> string

(** [git apply --cached] of one hunk from the UNSTAGED diff. *)
val stage_hunk : path:string -> raw:string -> unit Or_error.t Deferred.t

(** [git apply --cached --reverse] of one hunk from the STAGED diff. *)
val unstage_hunk : path:string -> raw:string -> unit Or_error.t Deferred.t
