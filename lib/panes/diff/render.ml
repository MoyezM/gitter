open! Core
open! Bonsai_term

(* Pure View production for the diff pane. Syntax owns the foreground; diff
   status owns the background tint. *)

let seg attrs s = View.text ~attrs s

(* Two number columns (5 wide each) plus the 2-wide bar column. *)
let gutter_width = 12

type content =
  [ `Message of string
  | `Document of Document.t * Highlight.t * Highlight.t
  ]

(* What the pane should show. One arm owns freshness: only a result tagged
   with the CURRENT selection key (path AND side) renders; anything else is
   still loading. *)
let content ~selection ~(result : Fetch.result) : content =
  match selection with
  | None -> `Message "no file selected"
  | Some sel ->
    (match result with
     | Some (key, r) when [%equal: string * [ `Staged | `Unstaged ]] sel key ->
       (match r with
        | Error e -> `Message ("git error: " ^ Error.to_string_hum e)
        | Ok (payload : Fetch.payload) ->
          if Array.is_empty payload.document
          then
            if payload.binary_only
            then `Message "no text changes (binary or mode-only)"
            else `Message "no changes vs HEAD"
          else `Document (payload.document, payload.old_hl, payload.new_hl))
     | Some _ | None -> `Message "loading diff...")
;;

(* Render a line as syntax-colored spans over the row's background. The row
   is truncated then padded to exactly [width], so the tint runs the full
   pane and never overflows it. *)
let spans_view ~base ~spans ~width ~pan text =
  let default_fg = [ Attr.fg Theme.text ] in
  let piece ~fg s = seg (fg @ base) s in
  (* Pan: slice the leading columns off text and spans alike (byte-based,
     consistent with the rest of the column math). *)
  let text = if pan > 0 then String.drop_prefix text pan else text in
  let spans =
    if pan = 0
    then spans
    else
      List.filter_map spans ~f:(fun { Highlight.Span.start_col; end_col; capture } ->
        let start_col = Int.max 0 (start_col - pan)
        and end_col = end_col - pan in
        if end_col <= 0 then None else Some { Highlight.Span.start_col; end_col; capture })
  in
  (* Truncate to at most [width] CODEPOINTS. Each codepoint occupies at
     least one cell, so more can never fit — but cutting by BYTES would
     discard multibyte content that fits the cell budget, and could split a
     codepoint. Rows of wide glyphs may still overflow in cells; the layout
     clips those as before. *)
  let text =
    let n = String.length text in
    if n <= width
    then text
    else (
      let is_start i = Char.to_int text.[i] land 0xC0 <> 0x80 in
      let rec go i seen =
        if i >= n
        then text
        else if is_start i && seen = width
        then String.prefix text i
        else go (i + 1) (if is_start i then seen + 1 else seen)
      in
      go 0 0)
  in
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
  let pad =
    if len < width then [ piece ~fg:default_fg (String.make (width - len) ' ') ] else []
  in
  View.hcat (go 0 spans [] @ pad)
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

let render_line ~width ~old_spans ~new_spans ~cursor_here ~pan (line : Document.line) =
  match line with
  | File_header p -> seg Theme.header p
  | Hunk_header h -> hunk_rule ~width h
  | Diff_line (old_no, new_no, line) ->
    (* The cursor lives in the gutter: highlighted, brightened numbers —
       visible without fighting the row tints. *)
    let gutter_attrs =
      if cursor_here then [ Attr.fg Theme.text; Theme.selection_bg; Attr.bold ] else Theme.context
    in
    let gutter = seg gutter_attrs (number old_no ^ number new_no) in
    let content_width = Int.max 1 (width - gutter_width) in
    (* Removed lines highlight from the HEAD blob; added/context from the
       worktree blob — the per-row span lists arrive pre-selected by side. *)
    let bar, content =
      match line with
      | Git.Diff.Line.Added s ->
        ( seg Theme.added_bar "\u{258E} "
        , spans_view ~base:[ Attr.bg Theme.added_bg ] ~spans:new_spans ~width:content_width ~pan s )
      | Removed s ->
        ( seg Theme.removed_bar "\u{258E} "
        , spans_view ~base:[ Attr.bg Theme.removed_bg ] ~spans:old_spans ~width:content_width ~pan s )
      | Context s ->
        seg Theme.context "  ", spans_view ~base:[] ~spans:new_spans ~width:content_width ~pan s
    in
    View.hcat [ gutter; bar; content ]
;;

(* Per-row span lists for the visible slice, one highlight window per
   CONTIGUOUS run of diff rows (a hunk's slice). A single min..max window
   over the whole viewport would span the file-line gap between hunks and
   degenerate to a near-full-file query when two distant hunks are visible
   at once. *)
let span_lookups ~old_hl ~new_hl (visible : Document.line array) =
  let n = Array.length visible in
  let old_spans = Array.create ~len:n []
  and new_spans = Array.create ~len:n [] in
  let side_range ~lo ~hi side =
    let extend acc v =
      match acc, v with
      | acc, None -> acc
      | None, Some v -> Some (v, v)
      | Some (mn, mx), Some v -> Some (Int.min mn v, Int.max mx v)
    in
    let acc = ref None in
    for i = lo to hi - 1 do
      match visible.(i) with
      | Document.Diff_line (o, n, _) -> acc := extend !acc (side (o, n))
      | File_header _ | Hunk_header _ -> ()
    done;
    !acc
  in
  let flush ~lo ~hi =
    let fill spans hl side =
      match side_range ~lo ~hi side with
      | None -> ()
      | Some (from_line, to_line) ->
        let window = Highlight.window hl ~from_line ~to_line in
        for i = lo to hi - 1 do
          match visible.(i) with
          | Document.Diff_line (o, n, _) ->
            (match side (o, n) with
             | Some line -> spans.(i) <- Highlight.Window.line window line
             | None -> ())
          | File_header _ | Hunk_header _ -> ()
        done
    in
    fill old_spans old_hl fst;
    fill new_spans new_hl snd
  in
  let run_start = ref None in
  for i = 0 to n - 1 do
    match visible.(i), !run_start with
    | Document.Diff_line _, None -> run_start := Some i
    | Diff_line _, Some _ -> ()
    | (File_header _ | Hunk_header _), Some lo ->
      flush ~lo ~hi:i;
      run_start := None
    | (File_header _ | Hunk_header _), None -> ()
  done;
  Option.iter !run_start ~f:(fun lo -> flush ~lo ~hi:n);
  old_spans, new_spans
;;

let render ~(content : content) ~(model : State.Model.t) ~(dimensions : Dimensions.t) =
  match content with
  | `Message m -> seg Theme.context (" " ^ m)
  | `Document (doc, old_hl, new_hl) ->
    let height = dimensions.height in
    let scroll = State.clamp_scroll doc ~height model.scroll in
    let cursor = State.effective_cursor doc model ~height in
    let visible =
      Array.sub doc ~pos:scroll ~len:(Int.min height (Array.length doc - scroll))
    in
    let old_spans, new_spans = span_lookups ~old_hl ~new_hl visible in
    let bar = Scrollbar.view ~total:(Array.length doc) ~visible:height ~offset:scroll in
    let width = dimensions.width - if Option.is_some bar then 1 else 0 in
    let body =
      View.vcat
        (Array.to_list
           (Array.mapi visible ~f:(fun i line ->
              render_line
                ~width
                ~old_spans:old_spans.(i)
                ~new_spans:new_spans.(i)
                ~cursor_here:(i + scroll = cursor)
                ~pan:model.pan
                line)))
    in
    Scrollbar.attach ~width:dimensions.width ~bar body
;;
