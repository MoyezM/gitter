open! Core
open! Bonsai_term

(* A one-column vertical scrollbar for any windowed surface: panes hcat it
   as their rightmost column when content overflows, and drop it (using the
   full width) when everything fits. *)

let thumb ~total ~visible ~offset =
  if total <= visible || visible <= 0
  then None
  else (
    let len = Int.max 1 (visible * visible / total) in
    let max_offset = total - visible in
    let offset = Int.clamp_exn offset ~min:0 ~max:max_offset in
    let pos = (visible - len) * offset / max_offset in
    Some (pos, len))
;;

let view ~total ~visible ~offset =
  match thumb ~total ~visible ~offset with
  | None -> None
  | Some (pos, len) ->
    Some
      (View.vcat
         (List.init visible ~f:(fun i ->
            if i >= pos && i < pos + len
            then View.text ~attrs:[ Attr.fg Theme.dim ] "\u{2590}"
            else View.text ~attrs:[] " ")))
;;

(* Overlay [bar] as [body]'s rightmost column. zcat (not hcat) on purpose:
   rows with wide glyphs can come out a cell wider than their budget
   (byte-based padding), and appending would let one such row push the bar
   past the pane edge where the layout clips it away. *)
let attach ~width ~bar body =
  match bar with
  | None -> body
  | Some bar -> View.zcat [ View.pad ~l:(Int.max 0 (width - 1)) bar; body ]
;;
