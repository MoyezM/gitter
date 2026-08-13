open! Core

(** The viewport state machine: cursor and scroll over a [Document], pure.
    The component wires this through [state_machine_with_input] so every
    event transitions the CURRENT model — a trackpad flick delivers many
    wheel ticks per frame, and folding them through a per-frame snapshot
    would collapse the burst into one step.

    Behavioral contract (helix-feel): the cursor rests only on cursor rows
    (diff rows and the markers standing in for elided context);
    motions keep it 5 rows from the viewport edges (margin shrinks on tiny
    panes so it always stays visible); a wheel tick moves the VIEW by 3
    rows and never moves the cursor — the selection may sit off-screen,
    and the next motion reveals it again. *)

module Model : sig
  type t =
    { cursor : int
      (** a POSITION in the full file, not a visible row: stable across
          every mask change by construction, so no re-anchoring exists.
          The row it displays on is [effective_cursor] — itself when the
          mask shows it, the marker hiding it otherwise. *)
    ; scroll : int (** first visible document row *)
    ; pan : int (** horizontal column offset of the content area *)
    ; levels : (int * int) Int.Map.t
      (** the mask, PER elided run (keyed by first pre-image line): the
          ABSOLUTE context depth each (top, bottom) end is open to —
          uniform (5, 5) is git -U5. Empty preserves the reader's own
          [diff.context] exactly, and expanding one end of one run moves
          nothing anywhere else. *)
    ; search : Search.Prompt.t
    ; current_match : int option
      (** the position the last jump landed on — the "2" of "2/7"; reset
          by query edits and document swaps *)
    ; snapshot : ((int * int) Int.Map.t * int * int) option
      (** (levels, cursor, scroll) at prompt-open — what Esc restores
          (L4); dropped on commit (D5) *)
    }

  val initial : t
end

module Action : sig
  type t =
    | Move of int (** cursor by n rows (snapped to a diff row) *)
    | Half_page of int (** direction: +1 down, -1 up *)
    | Wheel of int (** direction: +1 down, -1 up *)
    | Click of
        { row : int
          (** ABSOLUTE document row: the handler maps the clicked viewport
              row through the scroll it painted with, so clicks resolve
              against what the user saw even if scroll actions share the
              frame *)
        ; column : int
          (** pane-local; on a counted rule its chevrons choose the
              direction, anywhere else on the rule opens both ends *)
        }
    | Pan of int
        (** direction: +1 right, -1 left; 4-column steps, clamped just past
            the longest visible line; the gutter never pans *)
    | Context of [ `Up | `Down | `Open | `Reset ]
        (** [`Up] reveals more directly above the reading position — the
            bottom end of the boundary at or above the cursor — and
            [`Down] mirrors it; [`Open] (a click) raises both ends of the
            clicked rule; [`Reset] folds the run at the cursor. Steps walk
            a ladder of rungs, skipping any at or below what git already
            shipped, and resolve at APPLY time, so a burst walks the
            ladder. *)
    | Reveal
        (** after a doc replacement under the same selection: re-snap the
            kept cursor and reveal it. The expansion needs no repair — it
            is keyed on file lines. *)
    | Reset
    | Operate of [ `Stage_hunk | `Unstage_hunk | `Copy_line ]
        (** effectful keys, resolved at apply time by the component's
            wrapper (burst-safe) *)
  [@@deriving sexp_of]
end

(** Pure transition over the SOURCE, not a built document (the machine
    materializes what [model.levels] displays itself, so a burst of
    reveals in one frame sees each other's rows), and over the WRAPPED
    action type ([Search.Action] owns mode dispatch; jumps auto-reveal
    minimally and locally (D4) and land the cursor on the match —
    everything composes (D7)). Scroll is re-anchored against the
    current document and height first; motions start from
    [effective_cursor]. *)
val apply_action
  :  Document.Source.t
  -> Model.t
  -> Action.t Search.Action.t
  -> height:int
  -> Model.t

(** Render and the [Operate] wrapper both go through this, so no caller
    reads a document that disagrees with the model transitioning it. *)
val shown : Document.Source.t -> Model.t -> Document.t

(** The chevrons a counted rule draws after its number columns, and the
    click zones over them — one definition for both, so an arrow can never
    be clickable where it is not drawn. *)
val arrows : string

val arrows_cells : int

(** The VISIBLE row the cursor's file position is on — itself when the
    mask shows it, the counted rule hiding it otherwise. Independent of
    the viewport: wheel scrolling may leave it off-screen, and motions
    and staging still act on it. *)
val effective_cursor : Document.t -> Model.t -> int

(** Scroll clamped for the current document and pane height; render clamps
    with exactly this before slicing. *)
val clamp_scroll : Document.t -> height:int -> int -> int

(** The y target: "path:LINE" for the cursor row, bare path without one. *)
val yank_target : Document.t -> Model.t -> path:string -> string

(** The live query (typed while active, else the committed register),
    parsed — what render underlines (D2). *)
val search_query : Model.t -> Search.Query.t option

(** The border counter (D6): ["7 · 4 hidden"] before a jump, ["2/7 ·
    4 hidden"] once one establishes currency, hidden omitted at zero —
    the total ALWAYS includes hidden matches. [None] when no query is
    live; ["0"] on an empty or matchless document (D8). *)
val search_counts : Document.Source.t -> Model.t -> string option
