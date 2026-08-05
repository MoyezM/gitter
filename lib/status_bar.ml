open! Core
open! Bonsai_term

(* The one-row bar at the bottom: app name and branch state on the left,
   contextual key hints on the right. A notification (red error / dim
   info) TAKES OVER the right slot while present; when it clears, the
   hints — derived live from the focused pane — return, so whatever they
   say now is what comes back. Owns no state — the root publishes the
   facts. *)

let seg attrs s = View.text ~attrs s

let render ~left ~notice ~right ~width =
  let right, right_attrs =
    match notice with
    | Some (n, `Info) -> n ^ " ", [ Attr.fg Theme.dim; Attr.bg Theme.surface ]
    | Some (n, `Error) -> n ^ " ", [ Attr.fg Theme.red; Attr.bg Theme.surface ]
    | None -> right, Theme.status_bar
  in
  let used = String.length left + String.length right in
  let gap = width - used in
  if gap >= 1
  then
    View.hcat
      [ seg Theme.status_bar left
      ; seg Theme.status_bar (String.make gap ' ')
      ; seg right_attrs right
      ]
  else (
    (* Cramped: keep the left facts, truncate the rest. *)
    let line = String.prefix (left ^ " " ^ right) (Int.max 1 width) in
    seg Theme.status_bar (line ^ String.make (Int.max 0 (width - String.length line)) ' '))
;;
