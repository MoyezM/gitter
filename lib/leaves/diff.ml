open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The Diff leaf: shows the selected file's change vs HEAD. Fetches whenever
   the selection changes; a path tag on the stored result guards against
   out-of-order responses from rapid cursor movement (a stale fetch landing
   after a newer one renders as "loading" rather than the wrong file). *)

let seg attrs s = View.text ~attrs s
let pad_to width s = s ^ String.make (Int.max 0 (width - String.length s)) ' '

type display_line =
  | File_header of string
  | Hunk_header of string
  | Diff_line of int option * int option * Git.Diff.Line.t

let flatten (files : Git.Diff.File.t list) =
  let many = List.length files > 1 in
  List.concat_map files ~f:(fun file ->
    (if many then [ File_header file.path ] else [])
    @ List.concat_map file.hunks ~f:(fun hunk ->
      Hunk_header hunk.header
      :: List.map (Git.Diff.Hunk.numbered hunk) ~f:(fun (o, n, l) ->
        Diff_line (o, n, l))))
;;

(* Helix-style rows: dim old/new line-number gutters, a colored bar marking
   the change kind, then the line itself — no +/- prefixes. *)
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

(* Hunk boundaries as a quiet separator: the gutters already carry the line
   numbers, so only the function context is worth showing. *)
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
  | Diff_line (old_no, new_no, line) ->
    let gutter = seg Theme.context (number old_no ^ number new_no) in
    (* The tint hugs the code's extent (one trailing space) rather than
       filling the row — solid full-width bands sit heavily on translucent
       terminal backgrounds. *)
    ignore width;
    let bar, text =
      match line with
      | Added s -> seg Theme.added_bar "\u{258E} ", seg Theme.added_row (s ^ " ")
      | Removed s -> seg Theme.removed_bar "\u{258E} ", seg Theme.removed_row (s ^ " ")
      | Context s -> seg Theme.context "  ", seg [ Attr.fg Theme.text ] s
    in
    View.hcat [ gutter; bar; text ]
;;

(* What the pane should show, staleness-checked. *)
let content ~selection ~result =
  match selection, result with
  | None, _ -> `Message "no file selected"
  | Some _, None -> `Message "loading diff..."
  | Some sel, Some (path, _) when not (String.equal sel path) -> `Message "loading diff..."
  | Some _, Some (_, Error e) -> `Message ("git error: " ^ Error.to_string_hum e)
  | Some _, Some (_, Ok files) ->
    (match flatten files with
     | [] -> `Message "no changes vs HEAD"
     | lines -> `Lines lines)
;;

let render ~selection ~result ~scroll ~(dimensions : Dimensions.t) =
  match content ~selection ~result with
  | `Message m -> seg Theme.context (" " ^ m)
  | `Lines lines ->
    let visible =
      List.filteri lines ~f:(fun i _ -> i >= scroll && i < scroll + dimensions.height)
    in
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
        let%bind.Effect diff =
          Effect.of_deferred_thunk (fun () -> Git.Queries.diff_file_vs_head path)
        in
        set_result (Some (path, diff))
  in
  Bonsai.Edge.on_change ~equal:[%equal: string option] ~callback selection graph;
  let view =
    let%arr selection and result and scroll and dimensions in
    render ~selection ~result ~scroll ~dimensions
  in
  let handler =
    let%arr selection and result and scroll and set_scroll and dimensions in
    let line_count =
      match content ~selection ~result with
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
