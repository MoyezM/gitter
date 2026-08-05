open! Core
open Async

(* The queries the UI actually asks: run git, parse with the pure parsers. *)

let status () =
  let%map.Deferred.Or_error output = Runner.git [ "status"; "--porcelain=v2" ] in
  Status.parse output
;;

(* The file's content at HEAD — the old side of an uncommitted diff, used
   for old-side syntax highlighting. *)
let file_at_head path = Runner.git [ "show"; "HEAD:" ^ path ]

(* The file's combined (staged + unstaged) change vs HEAD — what "what did I
   change" means in the uncommitted view. Untracked files have no diff vs
   HEAD, so fall back to a --no-index diff against /dev/null (which exits 1
   when the file is non-empty, hence [accept_nonzero_exit]). *)
(* Parsing runs in a domain: a 530k-line diff (whole-file-added 17MB) costs
   ~110ms, which would stall ~7 frames on the scheduler. *)
let parse_off_thread output =
  let%map.Deferred files = Cpu.in_domain (fun () -> Diff.parse output) in
  Ok files
;;

let diff_file_vs_head path =
  match%bind.Deferred.Or_error
    Runner.git [ "diff"; "--no-color"; "HEAD"; "--"; path ]
  with
  | "" ->
    let%bind.Deferred.Or_error output =
      Runner.git
        ~accept_nonzero_exit:[ 1 ]
        [ "diff"; "--no-color"; "--no-index"; "--"; "/dev/null"; path ]
    in
    parse_off_thread output
  | output -> parse_off_thread output
;;
