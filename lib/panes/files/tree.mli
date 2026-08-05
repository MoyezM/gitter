open! Core

(** The status entries as a displayable tree: one [row] per rendered line,
    in git's path-sorted order, with collapsed directories contributing
    their Dir row but no children. *)

type row =
  | Dir of
      { path : string (** full directory path — the collapse key *)
      ; name : string (** the displayed segment *)
      ; depth : int
      ; expanded : bool
      }
  | File of
      { entry : Git.Status.Entry.t
      ; name : string (** path remainder below the parent dir (basename) *)
      ; depth : int
      }

val depth : row -> int

(** Build the visible rows. [collapsed] holds full directory paths. Entries
    may arrive in any order (porcelain emits tracked and untracked entries
    as two separately-sorted sections); rows are path-sorted so each
    directory appears exactly once. *)
val rows : entries:Git.Status.Entry.t list -> collapsed:String.Set.t -> row list
