open! Core

module Model = struct
  type t =
    { cursor : int (* a POSITION in the full file, not a visible row *)
    ; scroll : int
    ; pan : int (* horizontal column offset of the content area *)
    ; levels : (int * int) Int.Map.t
      (* per elided run (keyed by first pre-image line), how far its (top,
         bottom) ends are open beyond what git shipped; absent = git's
         own *)
    }

  let initial = { cursor = 0; scroll = 0; pan = 0; levels = Int.Map.empty }
end

module Action = struct
  type t =
    | Move of int
    | Half_page of int
    | Wheel of int
    | Click of
        { row : int (* ABSOLUTE document row (mapped from the painted scroll) *)
        ; column : int (* pane-local; hit-tested against [arrow_zone] on a rule *)
        }
    | Pan of int (* direction: +1 right, -1 left *)
    | Context of [ `Up | `Down | `Open | `Reset ]
    | Reveal (* after a doc replacement: re-snap the kept cursor, show it *)
    | Reset
    | Operate of [ `Stage_hunk | `Unstage_hunk | `Copy_line ]
      (* effectful keys, resolved at APPLY time by the component's
         apply_action wrapper (burst-safe) *)
  [@@deriving sexp_of]
end

(* Horizontal panning: a flat few columns per press. *)
let pan_step = 4

(* The view keeps the cursor this many rows from its edges when following. *)
let scrolloff = 5

(* Flat wheel step, shared with the listings (see Listing.wheel_step for
   the no-acceleration rationale). *)
let wheel_step = Listing.wheel_step

(* Context rungs, RELATIVE to what git shipped, so the first press always
   reveals ten lines whatever the reader's [diff.context]: "a bit more",
   "the function", "the region", then everything. *)
let rungs = [ 10; 30; 80; 200 ]

let step from ~base ~max =
  List.filter (List.map rungs ~f:(( + ) base) @ [ max ]) ~f:(fun r -> r > base && r <= max)
  |> List.find ~f:(fun r -> r > from)
  |> Option.value ~default:from
;;

(* Marker-row geometry: the chevrons drawn after the two number columns,
   and the click zones over them — ONE definition for both, so an arrow
   can never be clickable where it is not drawn (the original chevrons
   drew at one set of columns and hit-tested another, and clicking \u{25BC}
   expanded upward). Anchored at the pane's LEFT edge, so the scrollbar
   cannot shift them. *)
let arrows = "  \u{25B2}     \u{25BC}  "
let arrows_cells = 11 (* the cells [arrows] occupies: columns 10..20 *)

let arrow_zone ~column : [ `Up | `Down | `Row ] =
  if column >= 10 && column <= 15 then `Up else if column >= 16 && column <= 20 then `Down else `Row
;;

(* The document as [model] displays it — a pure function of the two, so
   render, the handler and this machine cannot disagree. *)
let shown source (model : Model.t) = Document.of_source source ~levels:model.levels

let clamp_row (doc : Document.t) i =
  Int.clamp_exn i ~min:0 ~max:(Int.max 0 (Array.length doc - 1))
;;

let clamp_scroll (doc : Document.t) ~height scroll =
  Listing.offset ~total:(Array.length doc) ~height scroll
;;

(* The cursor row nearest [i], preferring direction [dir]; falls back to the
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
      if j >= n then None else if Document.is_cursor_row doc.(j).line then Some j else fwd (j + 1)
    in
    let rec bwd j =
      if j < 0 then None else if Document.is_cursor_row doc.(j).line then Some j else bwd (j - 1)
    in
    let nearest =
      if dir >= 0
      then Option.first_some (fwd i) (bwd i)
      else Option.first_some (bwd i) (fwd i)
    in
    Option.value nearest ~default:i)
;;

(* The VISIBLE row the cursor is on: its own row when shown, the marker
   hiding it otherwise. Independent of the viewport: wheel scrolling may
   leave it off-screen, and motions and staging still act on it. *)
let effective_cursor (doc : Document.t) (model : Model.t) =
  snap doc ~dir:1 (Document.index_of_pos doc model.cursor)
;;

let pos_at (doc : Document.t) i = if Array.is_empty doc then 0 else doc.(clamp_row doc i).pos

(* View-follows-cursor with the scrolloff margin (Listing shrinks it on
   tiny panes so the bounds can't invert). *)
let follow (doc : Document.t) ~height ~cursor scroll =
  Listing.reveal ~margin:scrolloff ~total:(Array.length doc) ~height ~cursor scroll
;;

let move doc (model : Model.t) ~height ~by =
  let cursor = snap doc ~dir:(Int.compare by 0) (effective_cursor doc model + by) in
  let scroll = follow doc ~height ~cursor model.scroll in
  { model with Model.cursor = pos_at doc cursor; scroll }
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
    match doc.(i).line with
    | Document.Diff_line (_, _, (Added s | Removed s | Context s)) ->
      longest := Int.max !longest (String.length s)
    | File_header _ | Rule _ -> ()
  done;
  Int.max 0 (!longest - 8)
;;

(* Change the mask. The cursor is a file position, so it never moves; the
   VIEWPORT is what could jump, and it anchors to the reveal itself:

   - a directional reveal pins its ATTACHMENT side ([pin]): expanding up
     holds the content below the boundary and grows the view upward, and
     vice versa — the new rows always push AWAY from what you were
     reading toward;
   - otherwise (open-both, fold), the cursor's row keeps its exact screen
     line, fold-style; and when even that is off screen (parked by a
     wheel scroll), the top visible row holds.

   Never [follow]: [Context] is not a motion, and yanking the viewport
   back to an off-screen cursor is a jump, not a reveal. *)
let remask source (model : Model.t) ~height ~levels ~pin =
  if Map.equal [%equal: int * int] levels model.levels
  then model
  else (
    let before = shown source model in
    let after = Document.of_source source ~levels in
    let pinned p =
      let line = Document.index_of_pos before p - model.scroll in
      if line >= 0 && line < height
      then Some (Document.index_of_pos after p - line)
      else None
    in
    let model = { model with Model.levels = levels } in
    let scroll =
      match Option.bind pin ~f:pinned with
      | Some scroll -> scroll
      | None ->
        let offset = effective_cursor before model - model.scroll in
        if offset >= 0 && offset < height
        then effective_cursor after model - offset
        else Document.index_of_pos after (pos_at before model.scroll)
    in
    { model with Model.scroll = clamp_scroll after ~height scroll })
;;

let apply_action source (model : Model.t) (action : Action.t) ~height =
  (* Materializing here rather than in the graph is what keeps this
     burst-safe: a second [K] in one frame sees the first one's rows. *)
  let doc = shown source model in
  (* Resizes don't transition the machine, so scroll can be stale for the
     current height. Re-anchor before dispatch — Click in particular maps
     clicked screen rows through scroll and must agree with what render
     (which clamps identically) displayed. *)
  let model = { model with Model.scroll = clamp_scroll doc ~height model.scroll } in
  (* Expansion is LOCAL and DIRECTIONAL: [`Up] opens the bottom of the
     boundary above the cursor — the lines that appear directly above the
     reading position, github's ↑ — and [`Down] mirrors it. A click
     ([`Open]) raises both ends of the clicked rule; [`Reset] folds the
     run at the cursor. Only that run's levels move. *)
  let relevel ?cursor dir =
    let model =
      match cursor with
      | None -> model
      | Some pos -> { model with Model.cursor = pos }
    in
    let row = effective_cursor doc model in
    let run =
      match dir with
      | (`Up | `Down) as dir -> Document.run_toward source doc ~row ~dir
      | `Open | `Reset -> Document.run_at source doc ~row
    in
    match run with
    | None -> model
    | Some key ->
      let base = Document.base_context source in
      let max_t, max_b = Document.run_max source key in
      let t, b = Option.value (Map.find model.levels key) ~default:(0, 0) in
      let data =
        match dir with
        | `Reset -> 0, 0
        | `Up -> t, step b ~base ~max:max_b
        | `Down -> step t ~base ~max:max_t, b
        | `Open -> step t ~base ~max:max_t, step b ~base ~max:max_b
      in
      let levels =
        if [%equal: int * int] data (0, 0)
        then Map.remove model.levels key
        else Map.set model.levels ~key ~data
      in
      let pin =
        match dir, Document.run_span source key with
        | `Up, Some (_, last) -> Some (last + 1)
        | `Down, Some (first, _) -> Some (first - 1)
        | _ -> None
      in
      remask source model ~height ~levels ~pin
  in
  match action with
  | Action.Reset -> Model.initial
  | Move by -> move doc model ~height ~by
  | Half_page dir -> move doc model ~height ~by:(dir * height / 2)
  | Wheel dir -> wheel doc model ~height ~dir
  | Click { row; column } ->
    if Array.is_empty doc
    then model
    else (
      let row = clamp_row doc row in
      match doc.(row).line with
      (* The clicked rule becomes the cursor, so the anchor keeps the run
         you clicked at the pointer's screen line; its chevrons choose the
         direction, anywhere else on the rule opens both ends. *)
      | Document.Rule { hidden; _ } when hidden > 0 ->
        relevel
          ~cursor:(pos_at doc row)
          (match arrow_zone ~column with
           | `Up -> `Up
           | `Down -> `Down
           | `Row -> `Open)
      | _ -> { model with Model.cursor = pos_at doc (snap doc ~dir:1 row) })
  | Context dir -> relevel dir
  | Reveal ->
    (* A new document under the same selection: re-snap the kept cursor so
       staging targets something visible. [follow] is a no-op when it is
       already in view, so stage-and-resync doesn't move the viewport. *)
    let v = effective_cursor doc model in
    { model with Model.cursor = pos_at doc v; scroll = follow doc ~height ~cursor:v model.scroll }
  | Pan dir ->
    { model with
      Model.pan =
        Int.clamp_exn
          (model.pan + (pan_step * dir))
          ~min:0
          ~max:(max_pan doc ~height ~scroll:model.scroll)
    }
  | Operate _ -> model (* the component's wrapper schedules the effect *)
;;

(* path:LINE for the cursor row (worktree-side number, old side for
   deletions — the jump format editors accept); on a marker, the first
   line it is hiding. *)
let yank_target (doc : Document.t) (model : Model.t) ~path =
  if Array.is_empty doc
  then path
  else (
    match doc.(effective_cursor doc model).line with
    | Document.Diff_line (old_no, new_no, _) ->
      (match Option.first_some new_no old_no with
       | Some n -> sprintf "%s:%d" path n
       | None -> path)
    | Rule { hidden; new_no; _ } when hidden > 0 -> sprintf "%s:%d" path new_no
    | Rule _ | File_header _ -> path)
;;
