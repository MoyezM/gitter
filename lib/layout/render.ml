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

(* One leaf's renderable parts, as combined per frame by [Component]. *)
module Leaf_view = struct
  type t =
    { id : string
    ; title : string
    ; title_right : View.t option
    ; bottom : View.t option (* overlaid on the bottom border: the search line *)
    ; inner : View.t
    }
end

let render_pane ~focused ~title ~title_right ~bottom ~(rect : Geometry.Rect.t) inner =
  let attrs = if focused then Theme.border_focused else Theme.border in
  (* [Rect.inner] is the frame-inset law; match [leaf_dimensions]'s
     one-cell floor so content is clipped to what it was sized for. *)
  let inner_rect = Geometry.Rect.inner rect in
  let inner_w = Int.max 1 inner_rect.width in
  let inner_h = Int.max 1 inner_rect.height in
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
      else View.zcat [ View.pad ~l:(inner_rect.width - w) extra; boxed ]
  in
  (* The bottom-border mirror of [title_right]: the pane's search line,
     one cell in from the corner, floating over the border glyphs. *)
  let boxed =
    match bottom with
    | None -> boxed
    | Some extra ->
      View.zcat
        [ View.pad
            ~l:Geometry.Rect.frame
            ~t:(rect.height - Geometry.Rect.frame)
            (clip_to ~width:inner_w ~height:1 extra)
        ; boxed
        ]
  in
  (* The border box itself can also outgrow the rect (e.g. a title wider
     than the pane), so clip the finished box too. *)
  View.pad ~l:rect.x ~t:rect.y (clip_to ~width:rect.width ~height:rect.height boxed)
;;

let render ~(solved : Solver.Solved.t) ~leaf_views ~focused_id ~(dimensions : Dimensions.t) =
  let panes =
    List.filter_map solved.leaves ~f:(fun { id; rect } ->
      List.find leaf_views ~f:(fun (lv : Leaf_view.t) -> String.equal id lv.id)
      |> Option.map ~f:(fun { Leaf_view.id = _; title; title_right; bottom; inner } ->
        render_pane
          ~focused:(String.equal focused_id id)
          ~title
          ~title_right
          ~bottom
          ~rect
          inner))
  in
  View.zcat
    (panes
     @ [ View.transparent_rectangle ~width:dimensions.width ~height:dimensions.height ])
;;
