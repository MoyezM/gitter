open! Core
open Async

(* The queries the UI actually asks: run git, parse with the pure parsers. *)

let status ?working_dir () =
  let%map.Deferred.Or_error output =
    Runner.git ?working_dir [ "status"; "--porcelain=v2" ]
  in
  Status.parse output
;;

(* The working tree's diff for one file. [staged] selects the index side. *)
let diff_file ?working_dir ?(staged = false) path =
  let args =
    [ "diff"; "--no-color" ]
    @ (if staged then [ "--cached" ] else [])
    @ [ "--"; path ]
  in
  let%map.Deferred.Or_error output = Runner.git ?working_dir args in
  Diff.parse output
;;

(* The diff of the whole tree relative to [base] (merge-base semantics via
   three-dot syntax), for relative/review mode. *)
let diff_against ?working_dir ~base () =
  let%map.Deferred.Or_error output =
    Runner.git ?working_dir [ "diff"; "--no-color"; base ^ "..." ]
  in
  Diff.parse output
;;
