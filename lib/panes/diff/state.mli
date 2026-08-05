(** The viewport state machine: cursor and scroll over a [Document], pure.
    The component wires this through [state_machine_with_input] so every
    event transitions the CURRENT model — a trackpad flick delivers many
    wheel ticks per frame, and folding them through a per-frame snapshot
    would collapse the burst into one step.

    Behavioral contract (helix-feel): the cursor rests only on diff rows;
    motions keep it 5 rows from the viewport edges (margin shrinks on tiny
    panes so it always stays visible); a wheel tick moves the VIEW by 3 rows
    and drags the cursor along only when it would leave the viewport. *)

module Model : sig
  type t =
    { cursor : int (** document row index *)
    ; scroll : int (** first visible document row *)
    }

  val initial : t
end

module Action : sig
  type t =
    | Move of int (** cursor by n rows (snapped to a diff row) *)
    | Half_page of int (** direction: +1 down, -1 up *)
    | Wheel of int (** direction: +1 down, -1 up *)
    | Click of int (** viewport row that was clicked *)
    | Reset
  [@@deriving sexp_of]
end

(** Pure transition. [height] is the pane's inner height at dispatch time;
    scroll is re-anchored against the current document and height first
    (resizes don't fire actions), and motions start from
    [effective_cursor]. *)
val apply_action : Document.t -> Model.t -> Action.t -> height:int -> Model.t

(** The cursor row to DISPLAY and to start motions from: the model cursor
    when it rests on a diff row that is inside the viewport, else the first
    diff row visible in the viewport. The single owner of the "cursor sits
    on a visible diff line" invariant — render and apply_action must both
    use it, so what you see is always what [j] moves from. *)
val effective_cursor : Document.t -> Model.t -> height:int -> int

(** Scroll clamped for the current document and pane height; render clamps
    with exactly this before slicing. *)
val clamp_scroll : Document.t -> height:int -> int -> int
