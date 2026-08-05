open! Core
open Async

(* The single process edge of the git layer: everything else is pure
   parsing. Errors carry the argv for diagnosability. *)

let git ?accept_nonzero_exit args =
  match%map Process.run ?accept_nonzero_exit ~prog:"git" ~args () with
  | Ok output -> Ok output
  | Error err ->
    Error
      (Error.tag_s
         err
         ~tag:[%sexp "git command failed", { args : string list }])
;;
