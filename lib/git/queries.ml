open! Core
open Async

(* The queries the UI actually asks: run git, parse with the pure parsers. *)

let status () =
  (* -uall: enumerate files INSIDE untracked directories — the default
     collapses a fully-untracked dir into one "dir/" entry, which can't be
     diffed and hides its contents from review. *)
  let%map.Deferred.Or_error output =
    Runner.git [ "status"; "--porcelain=v2"; "--untracked-files=all" ]
  in
  Status.parse output
;;

(* The file's content at HEAD — the base of a staged diff. *)
let file_at_head path = Runner.git [ "show"; "HEAD:" ^ path ]

(* The file's content in the INDEX (stage 0) — the base of an unstaged
   diff, and the result side of a staged one. *)
let file_in_index path = Runner.git [ "show"; ":0:" ^ path ]

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

(* The UNSTAGED change: worktree vs index. Untracked files have no index
   entry (plain diff shows nothing for them), so fall back to a /dev/null
   whole-file-added diff — ls-files distinguishes them from files that are
   merely unchanged. *)
let diff_unstaged path =
  match%bind.Deferred.Or_error Runner.git [ "diff"; "--no-color"; "--"; Runner.literal path ] with
  | "" ->
    (match%bind.Deferred Sys.is_directory path with
     | `Yes -> Deferred.Or_error.return []
     | `No | `Unknown ->
       (match%bind.Deferred
          Runner.git [ "ls-files"; "--error-unmatch"; "--"; Runner.literal path ]
        with
        | Ok _ -> Deferred.Or_error.return [] (* tracked, genuinely unchanged *)
        | Error _ ->
          let%bind.Deferred.Or_error output =
            Runner.git
              ~accept_nonzero_exit:[ 1 ]
              [ "diff"; "--no-color"; "--no-index"; "--"; "/dev/null"; path ]
          in
          parse_off_thread output))
  | output -> parse_off_thread output
;;

(* The STAGED change: index vs HEAD. *)
let diff_staged path =
  let%bind.Deferred.Or_error output =
    Runner.git [ "diff"; "--no-color"; "--cached"; "--"; Runner.literal path ]
  in
  parse_off_thread output
;;
