open! Core
open! Bonsai_term

(* Helix-style which-key popup: thin square border titled with the current
   group's label, anchored to the bottom-right. Content is drawn plain and
   the whole panel gets ONE uniform background fill ([with_colors'
   ~fill_backdrop]) — never per-segment backgrounds, which read as stepped
   bands over the app. *)

let seg attrs s = View.text ~attrs s

let row ~width (key, label) =
  let padded = label ^ String.make (Int.max 0 (width - String.length label)) ' ' in
  View.hcat
    [ seg [ Attr.fg Theme.blue ] (sprintf " %c    " key)
    ; seg [ Attr.fg Theme.text ] (padded ^ " ")
    ]
;;

let render ~entries ~title ~(dimensions : Dimensions.t) =
  let items =
    List.map entries ~f:(fun e -> Commands.key e, Commands.label e)
    @ [ '?', "command palette" ]
  in
  let width =
    List.fold items ~init:0 ~f:(fun acc (_, label) -> Int.max acc (String.length label))
  in
  let rows = List.map items ~f:(row ~width) in
  let box =
    Bonsai_term_border_box.view
      ~line_type:Thin
      ~title
      ~title_attrs:[ Attr.fg Theme.text ]
      ~attrs:Theme.border
      (View.vcat rows)
    |> View.with_colors' ~fill_backdrop:true ~bg:Theme.popup
  in
  let box_width = width + 9 in
  let box_height = List.length rows + 2 in
  View.pad
    ~l:(Int.max 0 (dimensions.width - box_width - 1))
    ~t:(Int.max 0 (dimensions.height - box_height))
    box
;;
