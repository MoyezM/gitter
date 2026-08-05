(** The viewport state machine: cursor and scroll over a [Document], pure.
    The component wires this through [state_machine_with_input] so every
    event transitions the CURRENT model — a trackpad flick delivers many
    wheel ticks per frame, and folding them through a per-frame snapshot
    would collapse the burst into one step.

    Behavioral contract (helix-feel): the cursor rests only on diff rows;
    motions keep it 5 rows from the viewport edges (margin shrinks on tiny
    panes so it always stays visible); a wheel tick moves the VIEW by 3
    rows and never moves the cursor — the selection may sit off-screen,
    and the next motion reveals it again. *)

module Model : sig
  type t =
    { cursor : int (** document row index *)
    ; scroll : int (** first visible document row *)
    ; pan : int (** horizontal column offset of the content area *)
    }

  val initial : t
end

module Action : sig
  type t =
    | Move of int (** cursor by n rows (snapped to a diff row) *)
    | Half_page of int (** direction: +1 down, -1 up *)
    | Wheel of int (** direction: +1 down, -1 up *)
    | Click of int
        (** ABSOLUTE document row: the handler maps the clicked viewport
            row through the scroll it painted with, so clicks resolve
            against what the user saw even if scroll actions share the
            frame *)
    | Pan of int
        (** direction: +1 right, -1 left; 4-column steps, clamped just past
            the longest visible line; the gutter never pans *)
    | Reveal
        (** after a doc replacement under the same selection: re-snap the
            kept cursor into the new document and reveal it *)
    | Reset
  [@@deriving sexp_of]
end

(** Pure transition. [height] is the pane's inner height at dispatch time;
    scroll is re-anchored against the current document and height first
    (resizes don't fire actions), and motions start from
    [effective_cursor]. *)
val apply_action : Document.t -> Model.t -> Action.t -> height:int -> Model.t

(** The cursor row to display, to start motions from, and to stage hunks
    at: the model cursor normalized onto a diff row (fresh loads leave it
    on a header). Independent of the viewport — after wheel scrolling it
    may be off-screen, and render simply shows no cursor row. *)
val effective_cursor : Document.t -> Model.t -> int

(** Scroll clamped for the current document and pane height; render clamps
    with exactly this before slicing. *)
val clamp_scroll : Document.t -> height:int -> int -> int
