open! Core
open! Bonsai_term

(* A one-row bar composed at the app root (not inside Mode). It renders
   facts that other layers publish — mode name, focus label, menu breadcrumb
   — and owns no state of its own. *)

let render ~left ~right ~width =
  let gap = width - String.length left - String.length right in
  let line =
    if gap >= 1
    then left ^ String.make gap ' ' ^ right
    else if String.length left < width
    then left ^ String.make (width - String.length left) ' '
    else String.prefix left (Int.max 1 width)
  in
  View.text ~attrs:Theme.status_bar line
;;
