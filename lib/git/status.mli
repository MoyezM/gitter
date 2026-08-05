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

(** Parse [git diff --name-status] output into entries (letter in the
    index slot, worktree '.'; renames keep the new path). Total. *)
val parse_name_status : string -> Entry.t list
