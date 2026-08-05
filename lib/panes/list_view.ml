open! Core
open! Bonsai_term

(* The shared rendering shell of a scrollable listing: viewport slice,
   selection index, scrollbar. The per-row view stays with each pane. *)

let render
      ~(rows : 'r list)
      ~(scroll : int)
      ~(cursor : int)
      ~(dimensions : Dimensions.t)
      ~(row : selected:bool -> width:int -> 'r -> View.t)
  =
  let height = dimensions.height in
  let total = List.length rows in
  let offset = Listing.offset ~total ~height scroll in
  let bar = Scrollbar.view ~total ~visible:height ~offset in
  let width = dimensions.width - if Option.is_some bar then 1 else 0 in
  let body =
    View.vcat
      (List.take (List.drop rows offset) height
       |> List.mapi ~f:(fun i r -> row ~selected:(i + offset = cursor) ~width r))
  in
  Scrollbar.attach ~width:dimensions.width ~bar body
;;
