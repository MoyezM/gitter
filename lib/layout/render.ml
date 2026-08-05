open! Core
open! Bonsai_term

(* Pure rendering: solved geometry + leaf views in, one composed [View.t]
   out. Every pane is clipped to its solved rect, so a misbehaving leaf can
   never leak past its rectangle and occlude a neighbor. *)

(* Force [view] to exactly [width] x [height]: crop overflow, transparent-pad
   shortfall. *)
let clip_to ~width ~height view =
  let cropped =
    View.crop
      ~r:(Int.max 0 (View.width view - width))
      ~b:(Int.max 0 (View.height view - height))
      view
  in
  View.zcat [ cropped; View.transparent_rectangle ~width ~height ]
;;

let render_pane ~focused ~title ~title_right ~(rect : Geometry.Rect.t) inner =
  let attrs = if focused then Theme.border_focused else Theme.border in
  let inner_w = Int.max 1 (rect.width - 2) in
  let inner_h = Int.max 1 (rect.height - 2) in
  let boxed =
    Bonsai_term_border_box.view
      ~line_type:Round_corners
      ~title
      ~title_attrs:attrs
      ~attrs
      (clip_to ~width:inner_w ~height:inner_h inner)
  in
  (* Right-aligned title-bar extra (e.g. diffstat counts), overlaid on the
     top border one cell in from the corner; dropped when the pane is too
     narrow to keep it clear of the left title. *)
  let boxed =
    match title_right with
    | None -> boxed
    | Some extra ->
      let w = View.width extra in
      if w + String.length title + 6 > rect.width
      then boxed
      else View.zcat [ View.pad ~l:(rect.width - 2 - w) extra; boxed ]
  in
  (* The border box itself can also outgrow the rect (e.g. a title wider
     than the pane), so clip the finished box too. *)
  View.pad ~l:rect.x ~t:rect.y (clip_to ~width:rect.width ~height:rect.height boxed)
;;

let render ~(solved : Solver.Solved.t) ~leaf_views ~focused_id ~(dimensions : Dimensions.t) =
  let panes =
    List.filter_map solved.leaves ~f:(fun { id; rect } ->
      List.find leaf_views ~f:(fun (id', _, _, _) -> String.equal id id')
      |> Option.map ~f:(fun (_, title, title_right, inner) ->
        render_pane ~focused:(String.equal focused_id id) ~title ~title_right ~rect inner))
  in
  View.zcat
    (panes
     @ [ View.transparent_rectangle ~width:dimensions.width ~height:dimensions.height ])
;;
