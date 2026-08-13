open! Core

(** The prompt lifecycle, pane-agnostic: idle → [/] → active → Enter →
    committed (the visible register) → Esc → idle. Each searchable pane
    keeps one of these in its model; the HISTORY behind Ctrl-p/Ctrl-n
    is global ([History]), the register is per-pane (H3). Not quite
    pure: commits push onto that global stack, and recalls read it —
    transitions are not replayable (tests [History.clear] between
    scenarios).

    The pane owns everything content-shaped around this: the Esc
    snapshot (L4), what the query narrows or highlights, and when to
    take/restore/discard that snapshot on [Open]/[Cancel]/commits. *)

module Recall : sig
  (** An in-progress history walk: [prefix] is what was typed when the
      walk started (the filter and the text Ctrl-n returns to); [index]
      the current depth into the prefix-filtered stack. *)
  type t =
    { prefix : string
    ; index : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  { typed : string option (** [Some] while the prompt is active *)
  ; register : string option (** the committed query, faded in the border *)
  ; recall : Recall.t option
  }
[@@deriving sexp_of, equal]

val idle : t
val is_active : t -> bool

(** The query the pane should match against right now: the typed text
    while active (empty typed narrows to everything — the ghost is
    display-only), the register while committed, nothing while idle. *)
val query : t -> string option

(** [query], parsed. [Some []] is a live-but-empty query: trees show
    every row (Q4). *)
val parsed : t -> Query.t option

(** [parsed] with the empty query read as no query at all — the
    jump/count reading: nothing to walk, nothing to count. *)
val match_query : t -> Query.t option

type event =
  | Open (** [/]: open the prompt (register shown as ghost if any) *)
  | Type of string
    (** one typed printable, as its UTF-8 bytes — [utf8] encodes the
        non-ASCII ones; nothing printable is stolen (keys table) *)
  | Backspace (** deletes one CODEPOINT, so multibyte input round-trips *)
  | Commit
  (** Enter: typed text becomes the register (pushed to history); an
      empty prompt re-commits the ghost, or just closes without one *)
  | Implicit_commit
  (** a focus-stealing action (L3): as Commit, except an empty prompt
      closes WITHOUT re-committing the ghost *)
  | Cancel (** Esc while active: discard typed text, register unchanged *)
  | Clear (** Esc while committed: drop the register *)
  | Recall_prev
  | Recall_next
[@@deriving sexp_of]

val apply : t -> event -> t

(** The UTF-8 bytes of a typed non-ASCII printable ([Event.Key.Uchar]) —
    the byte-wise matcher takes them as-is. *)
val utf8 : Uchar.t -> string
