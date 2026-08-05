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

let fetch path =
  let open Async in
  Git.Queries.diff_file_vs_head path
  >>= function
  | Error _ as e -> return e
  | Ok files ->
    let%bind old_content =
      Git.Queries.file_at_head path
      >>| function
      | Ok c -> c
      | Error _ -> "" (* new/untracked file: no old side *)
    in
    let%bind new_content =
      Monitor.try_with (fun () -> Reader.file_contents path)
      >>| function
      | Ok c -> c
      | Error _ -> "" (* deleted file: no new side *)
    in
    return
      (Ok
         { files
         ; old_hl = Highlight.of_content ~path old_content
         ; new_hl = Highlight.of_content ~path new_content
         })
;;

type display_line =
  | File_header of string
  | Hunk_header of string
  | Diff_line of int option * int option * Git.Diff.Line.t * Highlight.Span.t list

let flatten { files; old_hl; new_hl } =
  let many = List.length files > 1 in
  List.concat_map files ~f:(fun file ->
    (if many then [ File_header file.path ] else [])
    @ List.concat_map file.hunks ~f:(fun hunk ->
      Hunk_header hunk.header
      :: List.map (Git.Diff.Hunk.numbered hunk) ~f:(fun (o, n, l) ->
        let spans =
          match l with
          | Git.Diff.Line.Removed _ -> Option.value_map o ~default:[] ~f:(Highlight.line old_hl)
          | Added _ | Context _ -> Option.value_map n ~default:[] ~f:(Highlight.line new_hl)
        in
        Diff_line (o, n, l, spans))))
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

let render_line ~width = function
  | File_header p -> seg Theme.header p
  | Hunk_header h -> hunk_rule ~width h
  | Diff_line (old_no, new_no, line, spans) ->
    let gutter = seg Theme.context (number old_no ^ number new_no) in
    (* Two number columns (5 wide each) plus the 2-wide bar column. *)
    let content_width = Int.max 1 (width - 12) in
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
    (match flatten payload with
     | [] -> `Message "no changes vs HEAD"
     | lines -> `Lines lines)
;;

let render ~lines ~scroll ~(dimensions : Dimensions.t) =
  match lines with
  | `Message m -> seg Theme.context (" " ^ m)
  | `Lines lines ->
    let visible = List.take (List.drop lines scroll) dimensions.height in
    View.vcat (List.map visible ~f:(render_line ~width:dimensions.width))
;;

let component ~(selection : string option Bonsai.t) : Widget.t =
  fun ~dimensions (local_ graph) ->
  let result, set_result = Bonsai.state None graph in
  let scroll, set_scroll = Bonsai.state 0 graph in
  let callback =
    let%arr set_result and set_scroll in
    fun (selection : string option) ->
      match selection with
      | None -> set_result None
      | Some path ->
        let%bind.Effect () = set_scroll 0 in
        let%bind.Effect diff = Effect.of_deferred_thunk (fun () -> fetch path) in
        set_result (Some (path, diff))
  in
  Bonsai.Edge.on_change ~equal:[%equal: string option] ~callback selection graph;
  (* Flattened once per fetch — NOT per scroll step; on a 100k-line diff
     recomputing this per event is the difference between smooth and
     frozen. *)
  let lines =
    let%arr selection and result in
    content ~selection ~result
  in
  let view =
    let%arr lines and scroll and dimensions in
    render ~lines ~scroll ~dimensions
  in
  let handler =
    let%arr lines and scroll and set_scroll and dimensions in
    let line_count =
      match lines with
      | `Message _ -> 0
      | `Lines lines -> List.length lines
    in
    let max_scroll = Int.max 0 (line_count - dimensions.height) in
    let scroll_by delta = set_scroll (Int.max 0 (Int.min max_scroll (scroll + delta))) in
    fun (event : Event.t) ->
      match event with
      | Event.Key_press { key = ASCII 'j'; mods = [] }
      | Key_press { key = Arrow `Down; mods = [] }
      | Mouse { kind = Scroll `Down; _ } -> scroll_by 1
      | Key_press { key = ASCII 'k'; mods = [] }
      | Key_press { key = Arrow `Up; mods = [] }
      | Mouse { kind = Scroll `Up; _ } -> scroll_by (-1)
      | _ -> Effect.Ignore
  in
  ~view, ~handler
;;
