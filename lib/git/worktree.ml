open! Core
open Async

(* The one WORKTREE-DESTRUCTIVE operation in the git layer. Every caller
   must put a confirmation in front of it — content discarded here is not
   recoverable through git. *)

let unit_result d = Deferred.map d ~f:(Or_error.map ~f:(fun (_ : string) -> ()))

(* Tracked changes revert via restore; untracked files/dirs under the path
   are then removed via clean. restore errors when the pathspec matches
   nothing tracked (e.g. a purely-untracked target) — ignore it and let
   clean do its half. *)
let discard_path path =
  let%bind (_ : string Or_error.t) =
    Runner.git [ "restore"; "--"; Runner.literal path ]
  in
  unit_result (Runner.git [ "clean"; "-f"; "-d"; "--"; Runner.literal path ])
;;
