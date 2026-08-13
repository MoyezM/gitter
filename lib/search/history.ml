open! Core

let entries : string list ref = ref []

let push query =
  if not (String.is_empty query)
  then (
    match !entries with
    | head :: _ when String.equal head query -> ()
    | _ -> entries := query :: !entries)
;;

let recall ~prefix n = List.nth (List.filter !entries ~f:(String.is_prefix ~prefix)) n

let clear () = entries := []
