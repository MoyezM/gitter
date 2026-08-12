open! Core

(** The diff as a MASK over the full file.

    Both blobs are already fetched (highlighting needs them), so the
    stable object is the whole file: every line of it, with removed lines
    interleaved, built ONCE per fetch. Alongside it, one integer per row:
    the distance to the nearest change. The whole feature is then a single
    law —

      visible  ⟺  dist ≤ base  ∨  up ≤ t (run)  ∨  down ≤ b (run)

    where [up]/[down] are the distances to the nearest change in each
    direction ([dist] is their min), [base] is the deepest context git
    itself shipped — RECOVERED from the data; we are never told
    [diff.context] — and each elided run carries its own (t, b): the top
    end opens toward the hunk above, the bottom toward the hunk below,
    independently. Expanding one end of one run moves nothing anywhere
    else. All levels 0 is git's own diff; a run with nothing left hidden
    loses its rule, so the merge falls out. Uniform symmetric levels
    reproduce [git diff -UN] byte-for-byte (property-tested against real
    output).

    Nothing else moves. Line numbers, highlight windows and hunk
    ownership all live in file coordinates, so a mask change cannot
    invalidate them — in particular the cursor is a POSITION in the full
    file, stable across every mask change by construction, and the hunk to
    stage is resolved from the cursor's line number on demand rather than
    stamped on rows. *)

type line =
  | File_header of string (** a file's path row (multi-file diffs only) *)
  | Rule of
      { hidden : int
        (** rows the mask is hiding here; 0 renders as the plain labeled
            rule between hunks, exactly as before this feature *)
      ; old_no : int (** first hidden pre-image line; the hunk's start when 0 hide *)
      ; new_no : int
      ; label : string (** the @@ line this rule stands for *)
      }
  | Diff_line of int option * int option * Git.Diff.Line.t
      (** old- and new-side line numbers; None on the side the line does
          not exist. Unmasked context arrives through this constructor too
          — it IS context, so highlighting, panning and yanking need no
          new case. *)

(** [revealed] marks a row git did not ship — equal to [dist > base] by
    the law above, cached for the renderer's rail. [pos] is the row's
    position in the full file: the currency cursors are held in; on a
    rule, the first position it hides. *)
type row =
  { line : line
  ; revealed : bool
  ; pos : int
  }

type t = row array

module Source : sig
  (** The full interleaved file, its distance field, and the parse it came
      from. Built off the UI thread by [Fetch].

      Interleaving succeeds only for single-file diffs whose @@ counts
      match their bodies, whose blob matches a sampled line, and whose
      gaps measure the same on both sides — a symlink target, LFS pointer
      or clean filter fails one of these and the source falls back to the
      plain flattened diff, which has nothing hidden and so never widens:
      exactly today's pane. *)
  type t

  val create : Git.Diff.File.t list -> old_text:string -> new_text:string -> t
  val empty : t


  (** A source yielding exactly these rows at every level — for fixtures,
      since the state machine transitions a source. *)
  val of_rows : row list -> t
end

(** The rows visible under the law at these per-run levels (runs are
    keyed by their first pre-image line — a content key, so a level
    survives refetches for untouched gaps and silently stops matching for
    staged-away ones). Memoized — render, the handler and every action ask
    for the same document, and a 646K-row diff must not be rebuilt per
    frame. *)
val of_source : Source.t -> levels:(int * int) Int.Map.t -> t

(** [base_context] is the recovered [diff.context]; [run_max key] the
    (t, b) past which each end of that run is fully open. An end's rungs
    live strictly between the two. [runs] lists the keys, for tests. *)
val base_context : Source.t -> int

val run_max : Source.t -> int -> int * int
val runs : Source.t -> int list

(** The elided run at [row] — its own when the row is a counted rule or a
    revealed line, else the nearest visible one, above first. What [X] and
    a click act on. *)
val run_at : Source.t -> t -> row:int -> int option

(** The run [K]/[J] act on: the row's own, else the nearest in that
    direction ALONE — "expand up" from inside a hunk reaches the boundary
    above it, never one below. *)
val run_toward : Source.t -> t -> row:int -> dir:[ `Up | `Down ] -> int option

(** The run's extent in file positions, for pinning a directional reveal
    to its attachment side. *)
val run_span : Source.t -> int -> (int * int) option

(** The visible row displaying position [pos]: the row itself when shown,
    the rule hiding it otherwise. How a cursor held in file positions is
    located in whatever the mask currently shows. *)
val index_of_pos : t -> int -> int

(** The hunk the row at [row] belongs to, resolved from its LINE NUMBERS
    against the original parse — never counted from display rows, and
    never stamped, so no mask change can shift it. Gap rows go to the
    nearer hunk; a counted rule to the hunk below it, a plain rule to its
    own. [None] out of range, on file headers, and for multi-file sources
    (which never stage). *)
val hunk_under : Source.t -> t -> row:int -> Git.Diff.Hunk.t option

(** Diff rows AND counted rules: a rule hiding something is a place you
    navigate onto and act on, which is how the affordance is reachable
    without a mouse. Plain rules are skipped, as they always were. *)
val is_cursor_row : line -> bool
