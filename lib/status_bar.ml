open! Core
open! Bonsai_term

(* The one-row bar at the bottom: app name and branch state on the left, a
   transient error notice (red) beside them, contextual key hints on the
   right. Owns no state — the root publishes the facts. *)

let seg attrs s = View.text ~attrs s

let render ~left ~notice ~right ~width =
  let notice_attrs =
    match notice with
    | Some ((_ : string), `Info) -> [ Attr.fg Theme.dim; Attr.bg Theme.surface ]
    | Some (_, `Error) | None -> [ Attr.fg Theme.red; Attr.bg Theme.surface ]
  in
  let notice = Option.map notice ~f:fst in
  let notice =
    match notice with
    | None -> ""
    | Some n -> "  " ^ n
  in
  let used = String.length left + String.length notice + String.length right in
  let gap = width - used in
  if gap >= 1
  then
    View.hcat
      [ seg Theme.status_bar left
      ; seg notice_attrs notice
      ; seg Theme.status_bar (String.make gap ' ')
      ; seg Theme.status_bar right
      ]
  else (
    (* Cramped: keep the left facts and the notice, drop hints. *)
    let line = String.prefix (left ^ notice) (Int.max 1 width) in
    seg Theme.status_bar (line ^ String.make (Int.max 0 (width - String.length line)) ' '))
;;
