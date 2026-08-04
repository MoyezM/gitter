open! Core
open! Bonsai_term

(* The command palette: a filterable flat list of every command with its
   bindings, helix-style — a query line with a match count, an underlined
   header, and a cursor-selectable list. Content is drawn plain; the whole
   panel gets one uniform background fill (see [Render]). *)

let seg attrs s = View.text ~attrs s

(* Fuzzy-ish: the query's characters must appear in the label, in order. *)
let matches ~query label =
  let label = String.lowercase label in
  let query = String.lowercase query in
  let rec go qi li =
    if qi >= String.length query
    then true
    else if li >= String.length label
    then false
    else if Char.equal query.[qi] label.[li]
    then go (qi + 1) (li + 1)
    else go qi (li + 1)
  in
  go 0 0
;;

let filter ~query flats =
  List.filter flats ~f:(fun (f : Commands.Flat.t) -> matches ~query f.label)
;;

let text_attrs = [ Attr.fg Theme.text ]
let dim_attrs = [ Attr.fg Theme.dim ]
let pad_to width s = s ^ String.make (Int.max 0 (width - String.length s)) ' '

let render ~filtered ~total ~query ~cursor ~(dimensions : Dimensions.t) =
  let width = Int.max 34 (Int.min 64 (dimensions.width - 6)) in
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
    List.filteri filtered ~f:(fun i _ -> i >= offset && i < offset + visible)
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
