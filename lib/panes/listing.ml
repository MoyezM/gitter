open! Core

(* The shared core of every scrollable, key-selected listing (the files
   panes, the stack pane; the diff pane borrows the viewport half). One
   owner for the laws this codebase kept re-implementing:

   - SELECTION is a key into the flattened traversal, stable under
     reorder; when its row disappears it repairs to the nearest surviving
     successor in the old key order, then predecessor, then None ([repair]
     — also what makes "stage a file" flow to the next one).
   - The VIEWPORT scrolls without moving the selection (the wheel), and
     selection motion reveals it with minimal scroll ([reveal]; a nonzero
     [margin] gives the diff pane's keep-away-from-the-edges follow).
   - Every transition snapshots the row keys so the next data change can
     repair against them. *)

module Model = struct
  type t =
    { selection : string option (* row key; None = nothing chosen yet *)
    ; scroll : int
    ; keys : string list (* the rows' keys at the last transition *)
    }

  let initial = { selection = None; scroll = 0; keys = [] }
end

(* Helix's scroll-lines default, flat — velocity comes from the terminal's
   event rate (more events per flick), so a fixed per-tick step already
   scrolls faster on faster flicks. *)
let wheel_step = 3

(* The viewport mapping: render and click handlers must agree on it or
   clicks activate the wrong row. *)
let offset ~total ~height scroll =
  Int.max 0 (Int.min scroll (total - Int.max 1 height))
;;

(* Scroll so [cursor] is visible, moving as little as possible; [margin]
   keeps it that many rows from either edge (shrunk on tiny panes so the
   bounds can't invert). [margin = 0] means: don't move at all while the
   cursor is anywhere on screen. *)
let reveal ?(margin = 0) ~total ~height ~cursor scroll =
  if height <= 0
  then scroll
  else (
    let margin = Int.min margin (Int.max 0 ((height - 1) / 2)) in
    scroll
    |> Int.max (cursor - (height - 1 - margin))
    |> Int.min (cursor - margin)
    |> Int.max 0
    |> offset ~total ~height)
;;

let index_of ~key rows selection =
  match selection with
  | None -> 0
  | Some k ->
    List.findi rows ~f:(fun _ r -> String.equal (key r) k)
    |> Option.value_map ~default:0 ~f:fst
;;

(* The visible parent of row [i]: the nearest earlier row one depth
   shallower (flattened trees grow down). *)
let parent_index ~depth rows i =
  match List.nth rows i with
  | None -> None
  | Some row ->
    let d = depth row in
    List.filter_mapi rows ~f:(fun j r -> Option.some_if (j < i && depth r = d - 1) j)
    |> List.last
;;

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

(* ---- transitions ------------------------------------------------------- *)

let empty = Model.initial

let wheel (model : Model.t) ~total ~height ~dir =
  let scroll =
    Int.clamp_exn
      (offset ~total ~height:1 model.scroll + (wheel_step * dir))
      ~min:0
      ~max:(Int.max 0 (total - Int.max 1 height))
  in
  { model with Model.scroll }
;;

(* Select the row at [index] (clamped) and snapshot the keys; with a
   [height], reveal it. *)
let select ~key rows ?height ~index (model : Model.t) =
  let total = List.length rows in
  let index = Int.clamp_exn index ~min:0 ~max:(Int.max 0 (total - 1)) in
  let selection = List.nth rows index |> Option.map ~f:key in
  let scroll =
    match height with
    | None -> offset ~total ~height:1 model.scroll
    | Some height -> reveal ~total ~height ~cursor:index model.scroll
  in
  { Model.selection; scroll; keys = List.map rows ~f:key }
;;

(* The rows changed underneath us: repair the selection, snapshot. *)
let rows_changed ~key rows (model : Model.t) =
  let new_keys = List.map rows ~f:key in
  { Model.selection = repair ~old_keys:model.keys ~selection:model.selection ~new_keys
  ; keys = new_keys
  ; scroll = offset ~total:(List.length rows) ~height:1 model.scroll
  }
;;
