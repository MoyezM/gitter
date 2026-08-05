open! Core
open Async

(* The single process edge of the git layer: everything else is pure
   parsing. Errors carry the argv for diagnosability. *)

let git ?stdin ?accept_nonzero_exit args =
  match%map Process.run ?stdin ?accept_nonzero_exit ~prog:"git" ~args () with
  | Ok output -> Ok output
  | Error err ->
    Error
      (Error.tag_s
         err
         ~tag:[%sexp "git command failed", { args : string list }])
;;

(* Paths after [--] are PATHSPECS: glob chars in a filename (pages/[id].tsx)
   would match OTHER files. The literal magic makes them plain filenames. *)
let literal path = ":(literal)" ^ path
