open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The Diff leaf: shows the selected file's change vs HEAD, syntax
   highlighted. Highlighting follows the full-file-then-slice practice: both
   blob contents are highlighted once and diff lines pull their side's spans
   by line number (removed lines from the HEAD blob, added/context from the
   worktree file). Syntax owns the foreground; diff status owns the
   background tint.

   A path tag on the stored result guards against out-of-order responses
   from rapid cursor movement. *)

let seg attrs s = View.text ~attrs s

type payload =
  { files : Git.Diff.File.t list
  ; old_hl : Highlight.t
  ; new_hl : Highlight.t
  }

(* Phase one of a fetch: the diff and both blob contents — no highlighting.
   The three reads are independent; start them all before binding
   (benchmarked at ~20-40ms each — serializing them tripled the latency
   floor of every fetch). *)
let fetch_text path =
  let open Async in
  let diff = Git.Queries.diff_file_vs_head path in
  let old_content =
    Git.Queries.file_at_head path
    >>| function
    | Ok c -> c
    | Error _ -> "" (* new/untracked file: no old side *)
  in
  let new_content =
    Monitor.try_with (fun () -> Reader.file_contents path)
    >>| function
    | Ok c -> c
    | Error _ -> "" (* deleted file: no new side *)
  in
  let%bind diff in
  let%bind old_content in
  let%bind new_content in
  match diff with
  | Error e -> return (Error e)
  | Ok files -> return (Ok (files, old_content, new_content))
;;

type display_line =
  | File_header of string
  | Hunk_header of string
  | Diff_line of int option * int option * Git.Diff.Line.t

(* The rows the cursor may rest on. *)
let is_diff_line = function
  | Diff_line _ -> true
  | File_header _ | Hunk_header _ -> false
;;

let flatten files =
  let many = List.length files > 1 in
  List.concat_map files ~f:(fun (file : Git.Diff.File.t) ->
    (if many then [ File_header file.path ] else [])
    @ List.concat_map file.hunks ~f:(fun hunk ->
      Hunk_header hunk.header
      :: List.map (Git.Diff.Hunk.numbered hunk) ~f:(fun (o, n, l) -> Diff_line (o, n, l))))
;;

(* Render a line as syntax-colored spans over the row's background, padded
   so the tint runs the full width of the pane. *)
let spans_view ~base ~spans ~width text =
  let default_fg = [ Attr.fg Theme.text ] in
  let piece ~fg s = seg (fg @ base) s in
  let len = String.length text in
  let rec go col spans acc =
    if col >= len
    then List.rev acc
    else (
      match spans with
      | [] -> List.rev (piece ~fg:default_fg (String.subo text ~pos:col) :: acc)
      | { Highlight.Span.start_col; end_col; capture } :: rest ->
        if end_col <= col
        then go col rest acc
        else if start_col > col
        then (
          let stop = Int.min start_col len in
          go stop spans (piece ~fg:default_fg (String.sub text ~pos:col ~len:(stop - col)) :: acc))
        else (
          let stop = Int.min end_col len in
          let fg = Option.value (Theme.syntax capture) ~default:default_fg in
          go stop rest (piece ~fg (String.sub text ~pos:col ~len:(stop - col)) :: acc)))
  in
  let pad = piece ~fg:default_fg (String.make (Int.max 1 (width - len)) ' ') in
  View.hcat (go 0 spans [] @ [ pad ])
;;

let number = function
  | Some n -> sprintf "%4d " n
  | None -> "     "
;;

(* The function context after the closing "@@", if any. *)
let hunk_context header =
  match String.substr_index ~pos:2 header ~pattern:"@@" with
  | Some i -> String.drop_prefix header (i + 2) |> String.strip
  | None -> ""
;;

let rule n = String.concat (List.init (Int.max 0 n) ~f:(Fn.const "\u{2500}"))

let hunk_rule ~width header =
  let context = hunk_context header in
  let label = if String.is_empty context then "" else " " ^ context ^ " " in
  View.hcat
    [ seg Theme.context (rule 4)
    ; seg (Attr.italic :: Theme.context) label
    ; seg Theme.context (rule (width - 4 - String.length label))
    ]
;;

let render_line ~width ~old_spans ~new_spans ~cursor_here = function
  | File_header p -> seg Theme.header p
  | Hunk_header h -> hunk_rule ~width h
  | Diff_line (old_no, new_no, line) ->
    (* The cursor lives in the gutter: highlighted, brightened numbers —
       visible without fighting the row tints. *)
    let gutter_attrs =
      if cursor_here then [ Attr.fg Theme.text; Theme.selection_bg; Attr.bold ] else Theme.context
    in
    let gutter = seg gutter_attrs (number old_no ^ number new_no) in
    (* Two number columns (5 wide each) plus the 2-wide bar column. *)
    let content_width = Int.max 1 (width - 12) in
    let spans =
      match line with
      | Git.Diff.Line.Removed _ -> Option.value_map old_no ~default:[] ~f:old_spans
      | Added _ | Context _ -> Option.value_map new_no ~default:[] ~f:new_spans
    in
    let bar, content =
      match line with
      | Added s ->
        ( seg Theme.added_bar "\u{258E} "
        , spans_view ~base:[ Attr.bg Theme.added_bg ] ~spans ~width:content_width s )
      | Removed s ->
        ( seg Theme.removed_bar "\u{258E} "
        , spans_view ~base:[ Attr.bg Theme.removed_bg ] ~spans ~width:content_width s )
      | Context s -> seg Theme.context "  ", spans_view ~base:[] ~spans ~width:content_width s
    in
    View.hcat [ gutter; bar; content ]
;;

(* What the pane should show, staleness-checked. *)
let content ~selection ~result =
  match selection, result with
  | None, _ -> `Message "no file selected"
  | Some _, None -> `Message "loading diff..."
  | Some sel, Some (path, _) when not (String.equal sel path) -> `Message "loading diff..."
  | Some _, Some (_, Error e) -> `Message ("git error: " ^ Error.to_string_hum e)
  | Some _, Some (_, Ok payload) ->
    (match flatten payload.files with
     | [] -> `Message "no changes vs HEAD"
     | lines -> `Lines (lines, payload.old_hl, payload.new_hl))
;;

(* The visible slice's line-number ranges on each side, for windowed
   highlight queries. *)
let side_ranges visible =
  List.fold visible ~init:(None, None) ~f:(fun (old_r, new_r) line ->
    match line with
    | Diff_line (o, n, _) ->
      let extend r v =
        match r, v with
        | r, None -> r
        | None, Some v -> Some (v, v)
        | Some (lo, hi), Some v -> Some (Int.min lo v, Int.max hi v)
      in
      extend old_r o, extend new_r n
    | File_header _ | Hunk_header _ -> old_r, new_r)
;;

let render ~lines ~cursor ~scroll ~(dimensions : Dimensions.t) =
  match lines with
  | `Message m -> seg Theme.context (" " ^ m)
  | `Lines (lines, old_hl, new_hl) ->
    let visible = List.take (List.drop lines scroll) dimensions.height in
    (* Freshly loaded diffs start with the cursor on the file header; show it
       on the first visible diff line instead of nowhere. *)
    let cursor =
      match List.nth visible (cursor - scroll) with
      | Some l when is_diff_line l -> cursor
      | _ ->
        (match List.findi visible ~f:(fun _ l -> is_diff_line l) with
         | Some (i, _) -> i + scroll
         | None -> cursor)
    in
    (* One ranged query per side per frame, over only the visible lines. *)
    let old_range, new_range = side_ranges visible in
    let lookup hl range =
      match range with
      | None -> fun _ -> []
      | Some (from_line, to_line) ->
        let window = Highlight.window hl ~from_line ~to_line in
        fun line -> Highlight.line_in_window window ~from_line line
    in
    let old_spans = lookup old_hl old_range in
    let new_spans = lookup new_hl new_range in
    View.vcat
      (List.mapi visible ~f:(fun i line ->
         render_line
           ~width:dimensions.width
           ~old_spans
           ~new_spans
           ~cursor_here:(i + scroll = cursor)
           line))
;;

(* --- Cursor and scrolling (helix-feel) ---------------------------------- *)

(* The view keeps the cursor this many lines from its edges when following. *)
let scrolloff = 5

(* Wheel: helix's scroll-lines default, flat — no acceleration here. Helix
   has none either: trackpad velocity reaches us as event RATE (the terminal
   converts pixel deltas into proportionally many wheel events), so a flat
   per-event step already scrolls faster when you flick faster. Adding our
   own boost on top double-accelerates and feels jumpy. *)
let wheel_step = 3

type view_state =
  { cursor : int
  ; scroll : int
  }

let initial_view_state = { cursor = 0; scroll = 0 }

(* The diff line nearest [i], preferring direction [dir]; falls back to the
   nearest one the other way; [i] itself when the document has none. *)
let snap lines ~dir i =
  let i = Int.clamp_exn i ~min:0 ~max:(Int.max 0 (List.length lines - 1)) in
  let idxs = List.filter_mapi lines ~f:(fun j l -> Option.some_if (is_diff_line l) j) in
  let fwd = List.find idxs ~f:(fun j -> j >= i) in
  let bwd = List.find (List.rev idxs) ~f:(fun j -> j <= i) in
  let first, second = if dir >= 0 then fwd, bwd else bwd, fwd in
  Option.first_some first second |> Option.value ~default:i
;;

(* Scroll so the cursor sits within scrolloff of neither edge. *)
let follow ~height ~count ~cursor scroll =
  let max_scroll = Int.max 0 (count - height) in
  let lo = cursor - (height - 1 - scrolloff) in
  let hi = cursor - scrolloff in
  scroll |> Int.max lo |> Int.min hi |> Int.max 0 |> Int.min max_scroll
;;

let move_cursor state lines ~height ~by =
  let count = List.length lines in
  let cursor = snap lines ~dir:(Int.compare by 0) (state.cursor + by) in
  let scroll = follow ~height ~count ~cursor state.scroll in
  { cursor; scroll }
;;

(* Wheel scrolls the VIEW; the cursor is then clamped to stay visible. *)
let wheel state lines ~height ~dir =
  let count = List.length lines in
  let step = wheel_step * dir in
  let scroll = Int.max 0 (Int.min (Int.max 0 (count - height)) (state.scroll + step)) in
  let cursor =
    state.cursor
    |> Int.max scroll
    |> Int.min (scroll + height - 1)
    |> snap lines ~dir
  in
  { cursor; scroll }
;;

let click state lines ~row = { state with cursor = snap lines ~dir:1 (state.scroll + row) }

(* Events transition the CURRENT model, not a per-frame snapshot: a trackpad
   flick delivers many wheel ticks per frame, and folding them through a
   captured state would collapse the whole burst into one step. *)
module View_action = struct
  type t =
    | Move of int
    | Half_page of int
    | Wheel of int
    | Click of int
    | Reset
  [@@deriving sexp_of]
end

let component ~(selection : string option Bonsai.t) : Widget.t =
  fun ~dimensions (local_ graph) ->
  let result, set_result = Bonsai.state None graph in
  (* Flattened once per fetch — NOT per scroll step; on a 100k-line diff
     recomputing this per event is the difference between smooth and
     frozen. *)
  let lines =
    let%arr selection and result in
    content ~selection ~result
  in
  let display_input =
    let%arr lines and dimensions in
    let display_lines =
      match lines with
      | `Message _ -> []
      | `Lines (lines, _, _) -> lines
    in
    display_lines, dimensions.height
  in
  let view_state, inject =
    Bonsai.state_machine_with_input
      ~default_model:initial_view_state
      ~apply_action:(fun _ctx input state (action : View_action.t) ->
        match input with
        | Bonsai.Computation_status.Inactive -> state
        | Active (lines, height) ->
          (match action with
           | Reset -> initial_view_state
           | Move by -> move_cursor state lines ~height ~by
           | Half_page dir -> move_cursor state lines ~height ~by:(dir * height / 2)
           | Wheel dir -> wheel state lines ~height ~dir
           | Click row -> click state lines ~row))
      display_input
      graph
  in
  let callback =
    let%arr set_result and inject in
    fun (selection : string option) ->
      match selection with
      | None -> set_result None
      | Some path ->
        let%bind.Effect () = inject Reset in
        let%bind.Effect fetched = Effect.of_deferred_thunk (fun () -> fetch_text path) in
        (match fetched with
         | Error e -> set_result (Some (path, Error e))
         | Ok (files, old_content, new_content) ->
           (* Two-phase: the diff text renders immediately (plain); the
              highlight sessions parse in their own domains and swap in when
              ready — frames never wait on a parse. *)
           let%bind.Effect () =
             set_result
               (Some (path, Ok { files; old_hl = Highlight.empty; new_hl = Highlight.empty }))
           in
           let%bind.Effect old_hl, new_hl =
             Effect.of_deferred_thunk (fun () ->
               Async.Deferred.both
                 (Highlight.create ~path old_content)
                 (Highlight.create ~path new_content))
           in
           set_result (Some (path, Ok { files; old_hl; new_hl })))
  in
  Bonsai.Edge.on_change ~equal:[%equal: string option] ~callback selection graph;
  let view =
    let%arr lines and view_state and dimensions in
    render ~lines ~cursor:view_state.cursor ~scroll:view_state.scroll ~dimensions
  in
  let handler =
    let%arr display_input and inject in
    let display_lines, _height = display_input in
    fun (event : Event.t) ->
      if List.is_empty display_lines
      then Effect.Ignore
      else (
        match event with
        | Event.Key_press { key = ASCII 'j'; mods = [] }
        | Key_press { key = Arrow `Down; mods = [] } -> inject (View_action.Move 1)
        | Key_press { key = ASCII 'k'; mods = [] }
        | Key_press { key = Arrow `Up; mods = [] } -> inject (Move (-1))
        (* Half-page jumps. *)
        | Key_press { key = ASCII ('n' | '['); mods = [] } -> inject (Half_page 1)
        | Key_press { key = ASCII ('p' | ']'); mods = [] } -> inject (Half_page (-1))
        | Mouse { kind = Scroll `Down; _ } -> inject (Wheel 1)
        | Mouse { kind = Scroll `Up; _ } -> inject (Wheel (-1))
        | Mouse { kind = Left; position; _ } -> inject (Click position.y)
        | _ -> Effect.Ignore)
  in
  ~view, ~handler
;;
