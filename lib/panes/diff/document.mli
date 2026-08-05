(** The flattened diff as a displayable document: one element per rendered
    row, array-backed so viewport slicing and row counting are O(height) and
    O(1) — a 646K-row diff must never be walked per frame or per event. *)

type line =
  | File_header of string (** a file's path row (multi-file diffs only) *)
  | Hunk_header of string (** the raw @@ line; rendered as a labeled rule *)
  | Diff_line of int option * int option * Git.Diff.Line.t
      (** old-side and new-side line numbers; None on the side where the
          line does not exist (added lines have no old number, removed lines
          no new number) *)

type t = line array

(** Build the document from parsed diff files. Called off the UI thread by
    [Fetch] — building is O(total lines). *)
val of_files : Git.Diff.File.t list -> t

(** The rows the cursor may rest on. *)
val is_diff_line : line -> bool

(** The (file index, hunk index) enclosing [row] — for staging the hunk
    under the cursor. None above the first hunk or on an empty document. *)
val hunk_at : t -> row:int -> (int * int) option
