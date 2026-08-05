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
    | Click of int (* ABSOLUTE document row (mapped from the painted scroll) *)
    | Pan of int (* direction: +1 right, -1 left *)
    | Reveal (* after a doc replacement: re-snap the kept cursor, show it *)
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

(* The cursor normalized onto a diff row (fresh loads leave it on a
   header). Deliberately independent of the viewport: wheel scrolling may
   leave the cursor off-screen, and motions/staging still act on it. *)
let effective_cursor (doc : Document.t) (model : Model.t) = snap doc ~dir:1 model.cursor

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
  let cursor = snap doc ~dir:(Int.compare by 0) (effective_cursor doc model + by) in
  let scroll = follow doc ~height ~cursor model.scroll in
  { model with Model.cursor; scroll }
;;

(* Wheel scrolls the VIEW only — the cursor stays put, off-screen if need
   be; the next motion reveals it again via [follow]. *)
let wheel doc (model : Model.t) ~height ~dir =
  { model with Model.scroll = clamp_scroll doc ~height (model.scroll + (wheel_step * dir)) }
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
  | Click row -> { model with Model.cursor = snap doc ~dir:1 row }
  | Reveal ->
    (* A new document landed under the same selection (revision bump):
       the kept cursor may now point outside it. Re-snap and reveal so
       both the highlight and hunk staging target something visible;
       [follow] is a no-op when the cursor is already in view, so the
       common stage-and-resync case doesn't move the viewport. *)
    let cursor = snap doc ~dir:1 model.cursor in
    { model with Model.cursor; scroll = follow doc ~height ~cursor model.scroll }
  | Pan dir ->
    { model with
      Model.pan =
        Int.clamp_exn
          (model.pan + (pan_step * dir))
          ~min:0
          ~max:(max_pan doc ~height ~scroll:model.scroll)
    }
;;
