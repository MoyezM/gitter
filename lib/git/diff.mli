(** Parser for unified diff output ([git diff --no-color]) — pure.
    Structure: files -> hunks -> lines. *)

module Line : sig
  type t =
    | Context of string
    | Added of string
    | Removed of string
  [@@deriving equal]
end

module Hunk : sig
  type t =
    { header : string (** the whole @@ line *)
    ; old_start : int (** first line number on the pre-image side *)
    ; old_count : int
      (** lines spanned on the pre-image side; the @@ count, defaulting to
          1 when omitted ("@@ -13 +13 @@"). A count of ZERO does not mean
          "an empty span at [old_start]" — git writes the line the hunk is
          anchored AFTER, so "-4,0" means "between old lines 4 and 5". Use
          [old_after]/[old_before] rather than doing the arithmetic. *)
    ; new_start : int (** first line number on the post-image side *)
    ; new_count : int
    ; lines : Line.t list
    ; raw : string
      (** the hunk verbatim, header through last body line (including
          [\ No newline] markers) — byte-exact input for [git apply] *)
    }

  (** Each line paired with its (old, new) line numbers; [None] on the side
      the line doesn't exist on. *)
  val numbered : t -> (int option * int option * Line.t) list

  (** The first line PAST the hunk and the last line BEFORE it, per side —
      i.e. the first and last line of the runs git elided around it. These
      exist so the count-zero convention above is honored in one place
      instead of at every call site. *)
  val old_after : t -> int

  val old_before : t -> int
  val new_after : t -> int
  val new_before : t -> int

  (** Does the parsed body deliver exactly what the @@ counts promised?
      The two are derived independently, so agreement is real evidence
      that neither the header nor the body parse lost a line. Consumers
      that measure elided runs off these numbers must check it first and
      fail closed — a disagreement means the numbers are untrustworthy,
      not that the hunk is unusable. *)
  val counts_agree : t -> bool
end

module File : sig
  type t =
    { path : string (** the post-image path *)
    ; hunks : Hunk.t list (** empty for binary or mode-only changes *)
    }
end

val parse : string -> File.t list

(** Parsed --numstat output: [(path, (added, removed))] per file, rename
    fields resolved to the NEW path, binary files dropped. Total. *)
val numstat : string -> (string * (int * int)) list
