open! Core
open! Bonsai_term

let cursor = "\u{258F}"

(* Tail-truncate to [cells], keeping the end (where the cursor is)
   visible behind a leading ellipsis. Query text is typed ASCII, so
   bytes are cells. *)
let fit ~cells s =
  let cells = Int.max 0 cells in
  if String.length s <= cells
  then s
  else if cells = 0
  then ""
  else "\u{2026}" ^ String.suffix s (cells - 1)
;;

let view ~(prompt : Prompt.t) ~counts ~width =
  let bright = [ Attr.fg Theme.text ] in
  let dim = Theme.context in
  (* The fixed decorations around the query text cost 4 cells in every
     mode. *)
  let fit = fit ~cells:(width - 4) in
  let query =
    match prompt.typed, prompt.register with
    | None, None -> None
    | Some "", Some ghost ->
      (* Empty active prompt: the register as a dim ghost after the
         cursor — display-only; Enter re-commits it (L2, R4). *)
      Some
        (View.hcat
           [ View.text ~attrs:bright (" /" ^ cursor)
           ; View.text ~attrs:dim (fit ghost)
           ; View.text " "
           ])
    | Some typed, _ ->
      Some
        (View.hcat
           [ View.text ~attrs:bright (" /" ^ fit typed)
           ; View.text ~attrs:bright (cursor ^ " ")
           ])
    | None, Some register ->
      Some (View.text ~attrs:dim (" \u{2315} " ^ fit register ^ " "))
  in
  Option.map query ~f:(fun query ->
    match counts with
    | None -> query
    | Some counts ->
      (* The counts drop before the query truncates (R2). *)
      let c = View.text ~attrs:dim (" " ^ counts ^ " ") in
      if View.width query + View.width c + 2 <= width
      then View.zcat [ query; View.pad ~l:(width - View.width c - 2) c ]
      else query)
;;
