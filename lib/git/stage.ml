open! Core
open Async

(* Index mutations — the ONLY writes in the git layer, and all of them
   index-only: none can touch worktree content. File/dir operations go
   through pathspecs; hunk operations construct a minimal patch from the
   parsed hunk's verbatim bytes and [git apply --cached] it. *)

let unit_result d = Deferred.map d ~f:(Or_error.map ~f:(fun (_ : string) -> ()))
let stage_path path = unit_result (Runner.git [ "add"; "--"; Runner.literal path ])

let unstage_path path =
  unit_result (Runner.git [ "restore"; "--staged"; "--"; Runner.literal path ])
;;

(* A one-hunk patch: git apply only needs the ---/+++ file headers and the
   hunk itself. [raw] must be the parser's verbatim hunk bytes. *)
let patch ~path ~raw = sprintf "--- a/%s\n+++ b/%s\n%s" path path raw

let apply_cached ~reverse ~path ~raw =
  let args = [ "apply"; "--cached" ] @ (if reverse then [ "--reverse" ] else []) @ [ "-" ] in
  unit_result (Runner.git ~stdin:(patch ~path ~raw) args)
;;

let stage_hunk ~path ~raw = apply_cached ~reverse:false ~path ~raw
let unstage_hunk ~path ~raw = apply_cached ~reverse:true ~path ~raw
