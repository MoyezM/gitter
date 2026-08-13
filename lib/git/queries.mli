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

(** The three text inputs of a file diff view: the parsed diff plus the
    (old, new) content pair, per side — the pairing law lives here.
    Unstaged diffs worktree vs index; Staged diffs index vs HEAD;
    Committed diffs merge-base-vs-[base] vs HEAD. A side the file doesn't
    have (untracked, deleted, new since HEAD) reads as ""; untracked
    files render as whole-file-added. The three reads run concurrently. *)
val diff_with_contents
  :  [ `Staged | `Unstaged | `Committed of string ]
  -> path:string
  -> (Diff.File.t list * string * string) Or_error.t Deferred.t

(** Raw for-each-ref over local heads — the poller's cheap change
    signature for ref moves ("error:..." on failure, still comparable).
    Only the probe; the stack itself comes from [Branch_stack.fetch]. *)
val refs_signature : unit -> string Deferred.t

(** Per-file (added, removed) line counts, (staged, unstaged) — from
    numstat; binary files skipped, untracked files not counted. *)
val diffstat
  :  unit
  -> ((int * int) String.Map.t * (int * int) String.Map.t) Or_error.t Deferred.t

(** What the branch adds over [base] (merge-base semantics): entries,
    per-file (added, removed) counts, and per-file (old blob, new blob) —
    the content-addressed review-mark key. *)
val committed
  :  base:string
  -> unit
  -> (Status.Entry.t list * (int * int) String.Map.t * (string * string) String.Map.t)
       Or_error.t
       Deferred.t

