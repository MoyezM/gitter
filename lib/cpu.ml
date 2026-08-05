open! Core

(* Run pure CPU work in its own domain so the Async scheduler (which is also
   the render loop) never stalls on it. [Domain.join] happens on a
   thread-pool thread, not the scheduler.

   Used for tree-sitter parsing (the C stubs never release the runtime
   lock — 1.1s for a 17MB file) and big diff parses (110ms for a 530k-line
   diff, benchmarked in bench/bench_diff.ml).

   OxCaml flags bare [Domain.spawn] (data-race freedom + domain-count
   pressure); uses here are safe: closures read only immutable inputs, and
   the domains are few and short-lived. *)
[@@@alert "-unsafe_multidomain-do_not_spawn_domains"]

let in_domain f =
  let domain = Domain.spawn f in
  Async.In_thread.run (fun () -> Domain.join domain)
;;
