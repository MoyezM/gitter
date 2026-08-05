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

(* Long content wraps and the box grows DOWN — never past its right edge. *)
let wrap ~width s =
  if String.length s <= width
  then [ s ]
  else
    List.init
      ((String.length s + width - 1) / width)
      ~f:(fun i -> String.sub s ~pos:(i * width) ~len:(Int.min width (String.length s - (i * width))))
;;

(* The typed text with the cursor glyph, wrapped; only the last few lines
   when it gets very long (the tail is where the cursor lives). *)
let input_lines ~width text =
  let lines = wrap ~width text in
  let lines =
    match List.rev lines with
    | last :: rest when String.length last < width -> List.rev ((last ^ "\u{258F}") :: rest)
    | rev_lines -> List.rev ("\u{258F}" :: rev_lines)
  in
  let max_lines = 6 in
  let n = List.length lines in
  if n > max_lines then List.drop lines (n - max_lines) else lines
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
      ([ line [ Attr.fg Theme.text; Attr.bold ] title; line [] "" ]
       @ List.map (wrap ~width body) ~f:(line [ Attr.fg Theme.dim ])
       @ [ line [] ""; line [ Attr.fg Theme.dim ] "y: confirm    n: cancel" ])
  | Input { title; text; _ } ->
    popup
      ~width
      ~dimensions
      ([ line [ Attr.fg Theme.text; Attr.bold ] title; line [] "" ]
       @ List.map (input_lines ~width text) ~f:(line [ Attr.fg Theme.text ])
       @ [ line [] ""; line [ Attr.fg Theme.dim ] "Enter: confirm    Esc: cancel" ])
;;
