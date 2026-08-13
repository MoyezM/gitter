(** Parser for [git status --porcelain=v2] — the machine-readable format
    with a stability guarantee. Pure string -> types. *)

module Entry : sig
  module Kind : sig
    type t =
      | Changed
      | Renamed of { from : string }
      | Untracked
      | Unmerged
    [@@deriving equal]
  end

  type t =
    { index : char (** raw X status letter; '.' means unmodified *)
    ; worktree : char (** raw Y status letter; '.' means unmodified *)
    ; path : string (** unquoted — real bytes, ready to hand back to git *)
    ; kind : Kind.t
    }

  (** The staged/unstaged classification, VSCode-style: the index side
      stages a file; the worktree side (or being untracked/conflicted)
      keeps it in Changes. A file with both kinds of changes is both. *)
  val is_staged : t -> bool

  val is_unstaged : t -> bool
end

(** Decode a git C-quoted path (core.quotePath); non-quoted input passes
    through unchanged. Total — malformed escapes degrade, never raise. *)
val unquote : string -> string

val parse : string -> Entry.t list

module Branch : sig
  type t =
    { head : string (** branch name; "(detached)" when detached *)
    ; ahead : int (** commits ahead of upstream; 0 without one *)
    ; behind : int
    }
end

(** Parsed from the [--branch] porcelain headers; None if they're absent. *)
val branch : string -> Branch.t option

(** Parse [git diff --raw --no-abbrev] output into entries (status letter
    in the index slot, worktree '.'; renames keep the new path) paired
    with (old blob, new blob) — the content-addressed review-mark key.
    Total. *)
val parse_raw : string -> (Entry.t * (string * string)) list
