open! Core

(** The one search grammar, everywhere: space-separated terms, ALL of
    which must hold; a leading [!] negates a term. A term matches by
    fuzzy subsequence — its characters must appear in the candidate, in
    order. Promoted from the command palette's greedy scan (which now
    consumes [matches]) and extended with spans, per-term smart-case,
    and negation. Byte-wise, like the original. *)

module Term : sig
  type t =
    { text : string (** the term without its leading [!] *)
    ; negated : bool
    ; fold_case : bool
      (** smart-case: no uppercase in [text] — match case-insensitively *)
    }
  [@@deriving sexp_of, compare, equal]
end

type t = Term.t list [@@deriving sexp_of, compare, equal]

(** A lone [!] is ignored (the user is mid-typing); [!] anywhere but
    term-start is a literal character. *)
val parse : string -> t

(** The empty query — it matches everything. *)
val is_empty : t -> bool

(** All terms hold: every positive term is a subsequence of the
    candidate, no negated term is. *)
val matches : t -> string -> bool

(** Byte positions the POSITIVE terms matched (sorted, deduped) — the
    underline channel. Negated terms filter but never highlight, so a
    match may have no positions. [None] when the query does not match. *)
val positions : t -> string -> int list option

(** [positions] merged into contiguous [(start, stop)] byte spans,
    [stop] exclusive. *)
val spans : t -> string -> (int * int) list option

(** Split [label] — the candidate's substring starting at byte [offset]
    — into [(text, underlined)] runs per [spans] (sorted, disjoint, in
    candidate coordinates). How a renderer turns match spans into
    per-segment attrs when it displays only part of the candidate
    (a basename, say). *)
val runs : spans:(int * int) list -> offset:int -> string -> (string * bool) list
