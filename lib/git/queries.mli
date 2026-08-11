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

(** Raw for-each-ref over local heads plus [extra] ref patterns — the
    poller's cheap change signature for ref moves. The caller passes the
    review target's remote refs when one is active (watching ALL of
    refs/remotes would turn every unrelated fetch into a full refresh).
    "error:..." on failure, still comparable. *)
val refs_signature : extra:string list -> unit -> string Deferred.t

(** The branch stack derived from git alone (heads + DAG above the common
    base + recent reflog tips) — see [Branch_stack]. [] when the repo has no
    branches. *)
val stack : unit -> Branch_stack.Branch.t list Or_error.t Deferred.t

(** Per-file (added, removed) line counts, (staged, unstaged) — from
    numstat; binary files skipped, untracked files not counted. *)
val diffstat
  :  unit
  -> ((int * int) String.Map.t * (int * int) String.Map.t) Or_error.t Deferred.t

(** What [head] adds over [base] (merge-base semantics): entries,
    per-file (added, removed) counts, and per-file (old blob, new blob) —
    the content-addressed review-mark key. [head] is "HEAD" for the
    self view, a branch (or origin/branch) under review otherwise. *)
val committed
  :  base:string
  -> head:string
  -> unit
  -> (Status.Entry.t list * (int * int) String.Map.t * (string * string) String.Map.t)
       Or_error.t
       Deferred.t

(** The file's content at [rev] — either side of a committed diff (the
    base side passes the merge-base sha the committed fetch resolved). *)
val file_at_rev : rev:string -> string -> string Or_error.t Deferred.t

(** The merge-base commit of two revs — resolved ONCE per committed
    fetch and carried in the diff key, so per-file loads don't re-derive
    it. *)
val merge_base : string -> string -> string Or_error.t Deferred.t

(** The committed change of [path] in [base...head]. *)
val diff_committed
  :  base:string
  -> head:string
  -> string
  -> Diff.File.t list Or_error.t Deferred.t

(** The ref a review should use for [name]: "origin/<name>" when that
    remote-tracking ref exists and [prefer_origin] (the pushed version
    is the reviewable truth), else [name]. Local ref inspection only —
    never touches the network. *)
val resolve_review_ref : prefer_origin:bool -> string -> string Deferred.t
