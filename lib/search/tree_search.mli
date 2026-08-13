open! Core
open! Bonsai_term

(** The whole search surface of a Listing-based tree pane — narrow while
    typing, jump when committed (T1-T8) — behind one facade. A pane is
    five row facts and a reveal (the [pane] record); its machine runs on
    ['a Action.t] (its own action type wrapped); everything else here:
    the lifecycle ([apply]), the displayed rows, the border line, the
    match underlining, the counters.

    The laws, per the PRD: while the prompt is active the pane shows
    matches plus their ancestor chain over the fully-expanded tree (T1,
    T2 — the fold state itself is untouched), the first matching row is
    selected live (T4), and zero matches hold the pre-prompt selection
    effective (T5). Commit un-narrows keeping the accepted selection,
    revealing whatever would hide it (T6); Cancel restores the
    (listing, fold) snapshot taken at open (L4); committed jumps walk
    the full tree, wrapping, revealing as they land (T7). *)

module Model : sig
  type 'fold t =
    { listing : Listing.Model.t
    ; fold : 'fold (** the pane's collapse state (dir set, override map) *)
    ; search : Prompt.t
    ; snapshot : (Listing.Model.t * 'fold) option (** what Esc restores *)
    }

  val initial : 'fold -> 'fold t
end

(** What a pane is, to the search: [rows] under a fold state; [all_rows]
    with every fold open (lazy — only filtering needs it); identity,
    display depth and ancestor-carrying per row; [candidate] the string
    a row matches on ([None]: not a match target — group rows);
    [reveal] the fold change that shows [key] (auto-expand, kept). *)
type ('row, 'fold) pane =
  { rows : 'fold -> 'row list
  ; all_rows : unit -> 'row list
  ; key : 'row -> string
  ; depth : 'row -> int
  ; is_parent : 'row -> bool
  ; candidate : 'row -> string option
  ; reveal : 'fold -> key:string -> 'fold
  }

(** The full transition: resolves mode-dependent keys, runs the search
    lifecycle, and hands the pane's own (already-resolved) actions to
    [apply_pane]. Hosts that schedule effects off pane actions
    [Action.resolve] first and pass the resolved action here — resolve
    is idempotent, so both see the same reading. *)
val apply
  :  ('row, 'fold) pane
  -> apply_pane:('fold Model.t -> 'a -> 'fold Model.t)
  -> 'fold Model.t
  -> 'a Action.t
  -> 'fold Model.t

(** The rows the pane displays right now — the single list render,
    clicks and index-carrying actions must agree on. Takes the two
    model fields it reads, so hosts derive from phys-stable projections
    and plain navigation rebuilds nothing. *)
val displayed_rows : ('row, 'fold) pane -> fold:'fold -> search:Prompt.t -> 'row list

(** (matching rows, total rows) over the full tree. (0, 0) when no
    query is live. *)
val match_counts : ('row, 'fold) pane -> Prompt.t -> int * int

(** Whether a jump would actually move — a register with a live match
    ([n] is a no-op otherwise, and hosts must not treat it as a
    selection claim). *)
val can_jump : ('row, 'fold) pane -> Prompt.t -> bool

(** The pane's bottom-border line: prompt/register plus the
    [matches/total] counter (R1, T5). [None] while idle. *)
val border : ('row, 'fold) pane -> search:Prompt.t -> width:int -> View.t option

(** [border] for hosted panes whose counts arrive as data — the one
    counter format, shared with [border]. *)
val border_of_counts
  :  search:Prompt.t
  -> counts:int * int
  -> width:int
  -> View.t option

(** The selection the search lifecycle falls back to: the snapshot's
    while a prompt is open, else the live one (T5/L4) — the same
    reading zero-match [live_select] uses. *)
val pre_prompt_selection : 'fold Model.t -> string option

(** [label] — the matched [candidate]'s trailing substring, as displayed
    — split into segments with the match spans underlined (T3, R3):
    underline is the match channel; syntax and status keep the colors.
    [query] is [Prompt.parsed search], computed ONCE per render — this
    runs per row. *)
val underline
  :  query:Query.t option
  -> attrs:Attr.t list
  -> candidate:string
  -> string
  -> View.t list
