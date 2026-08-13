open! Core
open! Bonsai_term

(* The command palette: a filterable flat list of every command with its
   bindings, helix-style — a query line with a match count, an underlined
   header, and a cursor-selectable list. Content is drawn plain; the whole
   panel gets one uniform background fill (see [Render]). *)

let seg attrs s = View.text ~attrs s

(* The one search grammar (Search.Query): the palette's original greedy
   subsequence scan lives there now, extended with terms, smart-case and
   negation — so `/` in a pane and `?` here feel like the same feature. *)
let filter ~query flats =
  let query = Search.Query.parse query in
  List.filter flats ~f:(fun (f : Commands.Flat.t) -> Search.Query.matches query f.label)
;;

let text_attrs = [ Attr.fg Theme.text ]
let dim_attrs = [ Attr.fg Theme.dim ]
let pad_to width s = s ^ String.make (Int.max 0 (width - String.length s)) ' '

let render ~filtered ~total ~query ~cursor ~(dimensions : Dimensions.t) =
  let width = Int.clamp_exn (dimensions.width - 6) ~min:34 ~max:64 in
  let label_col = width - 16 in
  let input_row =
    let count = sprintf "%d/%d " (List.length filtered) total in
    let typed = sprintf " %s\u{258F}" query in
    View.hcat
      [ seg text_attrs (pad_to (width - String.length count) typed)
      ; seg dim_attrs count
      ]
  in
  let header_row =
    View.hcat
      [ seg dim_attrs " "
      ; seg (Attr.underline :: dim_attrs) (pad_to (label_col + 1) "name")
      ; seg (Attr.underline :: dim_attrs) "bindings"
      ; seg dim_attrs (String.make (Int.max 0 (width - label_col - 10)) ' ')
      ]
  in
  let visible = Int.max 3 (Int.min (dimensions.height - 6) (List.length filtered)) in
  let offset = Int.max 0 (cursor - visible + 1) in
  let rows =
    List.take (List.drop filtered offset) visible
    |> List.mapi ~f:(fun i (f : Commands.Flat.t) ->
      let selected = i + offset = cursor in
      let marker = if selected then "> " else "  " in
      let attrs = if selected then Attr.bold :: text_attrs else text_attrs in
      View.hcat
        [ seg attrs (marker ^ pad_to label_col f.label)
        ; seg dim_attrs (pad_to 14 (Commands.Flat.bindings f))
        ])
  in
  let rows =
    if List.is_empty rows then [ seg dim_attrs (pad_to width " no matches") ] else rows
  in
  let box =
    Bonsai_term_border_box.view
      ~line_type:Thin
      ~attrs:Theme.border
      (View.vcat (input_row :: header_row :: rows))
    |> View.with_colors' ~fill_backdrop:true ~bg:Theme.popup
  in
  let box_width = width + 2 in
  View.pad ~l:(Int.max 0 ((dimensions.width - box_width) / 2)) ~t:1 box
;;
