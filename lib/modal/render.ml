open! Core
open! Bonsai_term

(* Centered popup, same visual language as the command palette: thin border,
   uniform near-black fill. *)

let seg attrs s = View.text ~attrs s
let pad_to width s = s ^ String.make (Int.max 0 (width - String.length s)) ' '

let popup ~width ~(dimensions : Dimensions.t) rows =
  let box =
    Bonsai_term_border_box.view
      ~line_type:Thin
      ~attrs:Theme.border
      (View.vcat (List.map rows ~f:(fun r -> View.hcat [ seg [] " "; r ])))
    |> View.with_colors' ~fill_backdrop:true ~bg:Theme.popup
  in
  View.pad
    ~l:(Int.max 0 ((dimensions.width - width - 2) / 2))
    ~t:(Int.max 1 (dimensions.height / 3))
    box
;;

let render (model : State.Model.t) ~(dimensions : Dimensions.t) =
  let width = Int.clamp_exn (dimensions.width - 10) ~min:30 ~max:70 in
  let line attrs s = seg attrs (pad_to width s) in
  match model with
  | Closed -> View.text ""
  | Confirm { title; body; _ } ->
    popup
      ~width
      ~dimensions
      [ line [ Attr.fg Theme.text; Attr.bold ] title
      ; line [] ""
      ; line [ Attr.fg Theme.dim ] body
      ; line [] ""
      ; line [ Attr.fg Theme.dim ] "y: confirm    n: cancel"
      ]
  | Input { title; text; _ } ->
    popup
      ~width
      ~dimensions
      [ line [ Attr.fg Theme.text; Attr.bold ] title
      ; line [] ""
      ; line [ Attr.fg Theme.text ] (text ^ "\u{258F}")
      ; line [] ""
      ; line [ Attr.fg Theme.dim ] "Enter: confirm    Esc: cancel"
      ]
;;
