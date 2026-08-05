open! Core
open Async

let run ~message =
  Deferred.map
    (Runner.git [ "commit"; "-m"; message ])
    ~f:(Or_error.map ~f:(fun (_ : string) -> ()))
;;
