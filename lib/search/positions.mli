open! Core

(** A memoized match-position set: which of an indexed collection of
    candidate lines match the query. One mutable slot, embedded in the
    owner (the diff's [Source] — a 646K-row file must not rescan per
    frame), with the typing-path refinement: a query that only SHRINKS
    the previous one's match set (appending to its last positive term,
    or appending whole terms) filters the memoized positions in
    O(matches) instead of rescanning. The empty query matches nothing.

    Owners embedding one of these must create it fresh per value — a
    [{ shared with ... }] record copy would alias the slot and serve
    another owner's cache (the codebase's memo-poisoning class). *)

type t

val create : unit -> t

(** Positions [i] in [0, length) whose [candidate i] matches [query],
    ascending. [candidate] returning [None] marks non-line rows (rules,
    file headers) — never matches. *)
val find
  :  t
  -> query:Query.t
  -> length:int
  -> candidate:(int -> string option)
  -> int array

(** The next/previous candidate from a SORTED ascending array, wrapping
    at the ends — the one walk behind every [n]/[N] and mid-prompt
    jump. [inclusive] admits [current] itself (the diff's first
    mid-prompt jump: "the first match at-or-after the cursor"); default
    is vim's strictly-past. [None] only when [candidates] is empty. *)
val next : ?inclusive:bool -> dir:int -> current:int -> int array -> int option
