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

(* The file's content at HEAD — the old side of an uncommitted diff, used
   for old-side syntax highlighting. *)
let file_at_head ?working_dir path = Runner.git ?working_dir [ "show"; "HEAD:" ^ path ]

(* The file's combined (staged + unstaged) change vs HEAD — what "what did I
   change" means in the uncommitted view. Untracked files have no diff vs
   HEAD, so fall back to a --no-index diff against /dev/null (which exits 1
   when the file is non-empty, hence [accept_nonzero_exit]). *)
let diff_file_vs_head ?working_dir path =
  match%bind.Deferred.Or_error
    Runner.git ?working_dir [ "diff"; "--no-color"; "HEAD"; "--"; path ]
  with
  | "" ->
    let%map.Deferred.Or_error output =
      Runner.git
        ?working_dir
        ~accept_nonzero_exit:[ 1 ]
        [ "diff"; "--no-color"; "--no-index"; "--"; "/dev/null"; path ]
    in
    Diff.parse output
  | output -> Deferred.Or_error.return (Diff.parse output)
;;

(* The diff of the whole tree relative to [base] (merge-base semantics via
   three-dot syntax), for relative/review mode. *)
let diff_against ?working_dir ~base () =
  let%map.Deferred.Or_error output =
    Runner.git ?working_dir [ "diff"; "--no-color"; base ^ "..." ]
  in
  Diff.parse output
;;
