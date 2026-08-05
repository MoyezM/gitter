open! Core

(* The selection-repair law shared by the list panes (files, stack).
   Selection is a KEY into a flattened traversal; rows reorder and shrink
   underneath it. The law: a surviving key is kept (stability under
   reorder); a vanished key moves to the nearest surviving SUCCESSOR in
   the old order, else the nearest surviving predecessor, else None —
   which is also what makes "stage a file" flow to the next one. *)

let repair ~old_keys ~selection ~new_keys =
  let survives k = List.mem new_keys k ~equal:String.equal in
  match selection with
  | None -> None
  | Some key when survives key -> Some key
  | Some key ->
    (match List.findi old_keys ~f:(fun _ k -> String.equal k key) with
     | None -> None
     | Some (i, _) ->
       let after = List.drop old_keys (i + 1) in
       let before = List.rev (List.take old_keys i) in
       Option.first_some (List.find after ~f:survives) (List.find before ~f:survives))
;;
