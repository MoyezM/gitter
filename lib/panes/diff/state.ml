open! Core

module Model = struct
  type t =
    { cursor : int
    ; scroll : int
    ; pan : int (* horizontal column offset of the content area *)
    }

  let initial = { cursor = 0; scroll = 0; pan = 0 }
end

module Action = struct
  type t =
    | Move of int
    | Half_page of int
    | Wheel of int
    | Click of int
    | Pan of int (* direction: +1 right, -1 left *)
    | Reset
  [@@deriving sexp_of]
end

(* Horizontal panning: a flat few columns per press. *)
let pan_step = 4

(* The view keeps the cursor this many rows from its edges when following. *)
let scrolloff = 5

(* Helix's scroll-lines default, flat — no acceleration here. Helix has none
   either: trackpad velocity reaches us as event RATE (the terminal converts
   pixel deltas into proportionally many wheel events), so a flat per-event
   step already scrolls faster when you flick faster. Adding our own boost
   on top double-accelerates and feels jumpy. *)
let wheel_step = 3

let clamp_row (doc : Document.t) i =
  Int.clamp_exn i ~min:0 ~max:(Int.max 0 (Array.length doc - 1))
;;

let clamp_scroll (doc : Document.t) ~height scroll =
  Int.clamp_exn scroll ~min:0 ~max:(Int.max 0 (Array.length doc - height))
;;

(* The diff row nearest [i], preferring direction [dir]; falls back to the
   nearest one the other way; [i] itself when the document has none. Total
   on empty documents (a binary-only diff yields [||], and queued events can
   still arrive against it). *)
let snap (doc : Document.t) ~dir i =
  let n = Array.length doc in
  if n = 0
  then 0
  else (
    let i = clamp_row doc i in
    let rec fwd j =
      if j >= n then None else if Document.is_diff_line doc.(j) then Some j else fwd (j + 1)
    in
    let rec bwd j =
      if j < 0 then None else if Document.is_diff_line doc.(j) then Some j else bwd (j - 1)
    in
    let nearest =
      if dir >= 0
      then Option.first_some (fwd i) (bwd i)
      else Option.first_some (bwd i) (fwd i)
    in
    Option.value nearest ~default:i)
;;

let effective_cursor (doc : Document.t) (model : Model.t) ~height =
  if Array.length doc = 0
  then 0
  else (
    let cursor = clamp_row doc model.cursor in
    let scroll = clamp_scroll doc ~height model.scroll in
    let stop = Int.min (Array.length doc) (scroll + height) in
    if cursor >= scroll && cursor < stop && Document.is_diff_line doc.(cursor)
    then cursor
    else (
      (* The model cursor is on a header (fresh load) or outside the
         viewport (resize leftovers, click-snap past the edge): show the
         cursor on — and move it from — the first diff row the user can
         actually see. *)
      let rec go j =
        if j >= stop then cursor else if Document.is_diff_line doc.(j) then j else go (j + 1)
      in
      go scroll))
;;

(* Scroll so the cursor sits within the margin of neither edge. The margin
   shrinks below [scrolloff] on tiny panes — at full scrolloff a pane under
   11 rows would invert the bounds and pin the cursor off-screen. *)
let follow (doc : Document.t) ~height ~cursor scroll =
  let margin = Int.min scrolloff (Int.max 0 ((height - 1) / 2)) in
  let lo = cursor - (height - 1 - margin) in
  let hi = cursor - margin in
  scroll |> Int.max lo |> Int.min hi |> clamp_scroll doc ~height
;;

let move doc (model : Model.t) ~height ~by =
  let cursor = snap doc ~dir:(Int.compare by 0) (effective_cursor doc model ~height + by) in
  let scroll = follow doc ~height ~cursor model.scroll in
  { model with Model.cursor; scroll }
;;

(* Wheel scrolls the VIEW; the cursor is then clamped to stay visible. *)
let wheel doc (model : Model.t) ~height ~dir =
  let scroll = clamp_scroll doc ~height (model.scroll + (wheel_step * dir)) in
  let cursor =
    model.cursor |> Int.max scroll |> Int.min (scroll + height - 1) |> snap doc ~dir
  in
  { model with Model.cursor; scroll }
;;

(* The pan limit: just past the longest line VISIBLE right now — panning
   into blank space beyond every visible line is disorienting. *)
let max_pan (doc : Document.t) ~height ~scroll =
  let stop = Int.min (Array.length doc) (scroll + height) in
  let longest = ref 0 in
  for i = scroll to stop - 1 do
    match doc.(i) with
    | Document.Diff_line (_, _, (Added s | Removed s | Context s)) ->
      longest := Int.max !longest (String.length s)
    | File_header _ | Hunk_header _ -> ()
  done;
  Int.max 0 (!longest - 8)
;;

let apply_action (doc : Document.t) (model : Model.t) (action : Action.t) ~height =
  (* Resizes don't transition the machine, so scroll can be stale for the
     current height. Re-anchor before dispatch — Click in particular maps
     clicked screen rows through scroll and must agree with what render
     (which clamps identically) displayed. *)
  let model = { model with Model.scroll = clamp_scroll doc ~height model.scroll } in
  match action with
  | Action.Reset -> Model.initial
  | Move by -> move doc model ~height ~by
  | Half_page dir -> move doc model ~height ~by:(dir * height / 2)
  | Wheel dir -> wheel doc model ~height ~dir
  | Click row -> { model with Model.cursor = snap doc ~dir:1 (model.scroll + row) }
  | Pan dir ->
    { model with
      Model.pan =
        Int.clamp_exn
          (model.pan + (pan_step * dir))
          ~min:0
          ~max:(max_pan doc ~height ~scroll:model.scroll)
    }
;;
