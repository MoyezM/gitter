open! Core
open! Bonsai_term

type t = { generation : int ref }

let create () = { generation = ref 0 }

let start ?(on_start = fun () -> ()) t =
  Effect.of_thunk (fun () ->
    incr t.generation;
    on_start ();
    !(t.generation))
;;

let observe t = Effect.of_thunk (fun () -> !(t.generation))
let peek t = !(t.generation)

let when_current t token eff =
  let%bind.Effect current = observe t in
  if current = token then eff else Effect.Ignore
;;

let run ?on_start t ~fetch ~commit =
  let%bind.Effect token = start ?on_start t in
  let%bind.Effect result = Effect.of_deferred_thunk fetch in
  when_current t token (commit result)
;;

let invalidate t = Effect.of_thunk (fun () -> incr t.generation)
