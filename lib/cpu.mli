open! Core

(** Run pure CPU work in its own domain so the Async scheduler (which is
    also the render loop) never stalls on it.

    [fallback] is the caller's degradation policy for when the domain
    cannot spawn (domain exhaustion): it runs on the scheduler instead —
    slow but correct. It is a required argument so the policy cannot be
    forgotten at a call site. Exceptions raised by [f] itself still
    propagate through the returned deferred, as before. *)
val in_domain : fallback:(unit -> 'a) -> (unit -> 'a) -> 'a Async.Deferred.t
