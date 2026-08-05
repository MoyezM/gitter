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
    ; new_start : int (** first line number on the post-image side *)
    ; lines : Line.t list
    }

  (** Each line paired with its (old, new) line numbers; [None] on the side
      the line doesn't exist on. *)
  val numbered : t -> (int option * int option * Line.t) list
end

module File : sig
  type t =
    { path : string (** the post-image path *)
    ; hunks : Hunk.t list (** empty for binary or mode-only changes *)
    }
end

val parse : string -> File.t list
