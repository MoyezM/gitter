open! Core

module Model = struct
  type t =
    { cursor : int (* a POSITION in the full file, not a visible row *)
    ; scroll : int
    ; pan : int (* horizontal column offset of the content area *)
    ; levels : (int * int) Int.Map.t
      (* per elided run (keyed by first pre-image line), the ABSOLUTE
         context depth each (top, bottom) end is open to — the same unit
         as [dist], so (5, 5) everywhere is git -U5; absent = git's own.
         [step] keeps every rung above [base], which is why a stored end
         always exceeds what git shipped. *)
    ; search : Search.Prompt.t
    ; current_match : int option
      (* the position the last jump landed on — the "2" of "2/7"; None
         until a jump establishes currency, and again after query edits
         and document swaps (the next jump anchors at the cursor) *)
    ; snapshot : ((int * int) Int.Map.t * int * int) option
      (* (levels, cursor, scroll) at prompt-open — what Esc restores
         (L4); dropped on commit, so mid-prompt reveals become ordinary
         ones (D5) *)
    }

  let initial =
    { cursor = 0
    ; scroll = 0
    ; pan = 0
    ; levels = Int.Map.empty
    ; search = Search.Prompt.idle
    ; current_match = None
    ; snapshot = None
    }
  ;;
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

(* ---- search over the full file (D1-D9) --------------------------------- *)

let search_query (model : Model.t) = Search.Prompt.parsed model.search

let matches source (model : Model.t) =
  match Search.Prompt.match_query model.search with
  | None -> [||]
  | Some query -> Document.search_positions source ~query
;;

(* The border's counter (D6, R5): total matches — hidden ones included —
   then [current/total] once a jump establishes currency, and the hidden
   count as long as the screen can't show everything. Runs per frame
   while a query is live, so everything expensive sits behind the
   Document memos. *)
let search_counts source (model : Model.t) =
  match Search.Prompt.match_query model.search with
  | None -> None
  | Some query ->
    let ms = Document.search_positions source ~query in
    let total = Array.length ms in
    let hidden = Document.search_hidden source ~query ~levels:model.levels in
    let position =
      (* Nested match rather than [Option.bind]: [binary_search]'s
         result is stack-local under OxCaml and may not escape into a
         global-expecting closure. *)
      match
        match model.current_match with
        | None -> None
        | Some pos -> Array.binary_search ms `First_equal_to pos ~compare:Int.compare
      with
      | Some i -> sprintf "%d/%d" (i + 1) total
      | None -> Int.to_string total
    in
    Some (if hidden > 0 then sprintf "%s \u{00B7} %d hidden" position hidden else position)
;;

(* Jump to the next/previous match (D3, D4, D6): anchored at the cursor —
   inclusively for the first mid-prompt jump ("is it there?" before you
   go), strictly after it otherwise (vim's n) — wrapping at the ends. A
   hidden target reveals minimally and locally on arrival, and the jump
   is a MOTION: the viewport follows the cursor. *)
let jump source (model : Model.t) ~height ~dir =
  let inclusive =
    Search.Prompt.is_active model.search && Option.is_none model.current_match
  in
  match Search.Positions.next ~inclusive ~dir ~current:model.cursor (matches source model) with
  | None -> model
  | Some pos ->
    let levels = Document.reveal_pos source ~levels:model.levels ~pos in
    let doc = Document.of_source source ~levels in
    let cursor_row = Document.index_of_pos doc pos in
    { model with
      Model.levels
    ; cursor = pos
    ; current_match = Some pos
    ; scroll = follow doc ~height ~cursor:cursor_row model.scroll
    }
;;

let apply_search source (model : Model.t) (event : Search.Prompt.event) ~height =
  let was_active = Search.Prompt.is_active model.search in
  let search = Search.Prompt.apply model.search event in
  let model' = { model with Model.search } in
  match event with
  | Search.Prompt.Open ->
    if was_active
    then model'
    else
      { model' with
        Model.snapshot = Some (model.levels, model.cursor, model.scroll)
      ; current_match = None
      }
  | Type _ | Backspace | Recall_prev | Recall_next ->
    (* D3: typing updates highlights and counts only — the cursor and
       viewport hold still; currency resets (the next jump anchors at
       the cursor). *)
    { model' with Model.current_match = None }
  | Commit | Implicit_commit ->
    (* Mid-prompt reveals are kept and become ordinary ones (D5). *)
    { model' with Model.snapshot = None }
  | Cancel ->
    (match model.snapshot with
     | Some (levels, cursor, scroll) ->
       let doc = Document.of_source source ~levels in
       { model' with
         Model.snapshot = None
       ; levels
       ; cursor
       ; scroll = clamp_scroll doc ~height scroll
       ; current_match = None
       }
     | None -> { model' with Model.snapshot = None; current_match = None })
  | Clear -> { model' with Model.current_match = None }
;;

(* The pane's own transitions, over already-RESOLVED actions. The jump
   walk and mask-reveal live above; [Search.Action] owns mode dispatch. *)
let apply_plain source (model : Model.t) (action : Action.t) ~height =
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
  | Action.Reset ->
    (* A NEW document under a new selection wipes the viewport — but the
       register is a query, not a match set: it survives the swap and
       re-applies to the new content, currency reset (L1, D9). The
       snapshot does NOT survive: its (levels, cursor, scroll) are
       coordinates of the OLD document (levels keyed by the old source's
       runs), so a later Esc-cancel must not restore them onto the new
       one — [Model.initial] leaves it None. *)
    { Model.initial with Model.search = model.search }
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
       already in view, so stage-and-resync doesn't move the viewport.
       Currency resets — positions shifted with the content, so the old
       current match may name an unrelated line (L1/D9); the next jump
       anchors at the cursor. *)
    let v = effective_cursor doc model in
    { model with
      Model.cursor = pos_at doc v
    ; scroll = follow doc ~height ~cursor:v model.scroll
    ; current_match = None
    }
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

(* The full transition over the WRAPPED action type: [Search.Action]
   owns mode dispatch; jumps and the prompt lifecycle land above; the
   pane's own actions go to [apply_plain]. Search actions use the
   machine-input [height] (the pane's canonical), not the stamped one —
   same dimensions either way. *)
let apply_action source (model : Model.t) (action : Action.t Search.Action.t) ~height =
  match Search.Action.resolve ~search:model.search action with
  | None -> model
  | Some (Search.Action.Pane a) -> apply_plain source model a ~height
  | Some (Prompt { event; height = (_ : int) }) -> apply_search source model event ~height
  | Some (Jump { dir; height = (_ : int) }) -> jump source model ~height ~dir
  | Some (By_mode _) -> model (* [resolve] never returns one *)
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
