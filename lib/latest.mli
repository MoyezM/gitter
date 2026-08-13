open! Core
open! Bonsai_term

(** Latest-wins async writes: many attempts may start; only the NEWEST
    may land. The guard for every fetch-then-commit in the app — a slow
    stale result must never overwrite a fresh one.

    [run] is the whole story for single-write fetches. Multi-write
    protocols (e.g. the diff pane's two-phase load) and non-superseding
    observers (the poller) compose the token level directly: [start]
    (or [observe]) for a token, [when_current] around each write.

    The counter is honest mutable identity per [t] (not Bonsai state),
    read and written only inside effect thunks. *)
type t

val create : unit -> t

(** Begin a superseding attempt: bump the generation and return the new
    token. [on_start] runs inside the same thunk as the bump — for
    side-channel resets that must be atomic with it. *)
val start : ?on_start:(unit -> unit) -> t -> int Effect.t

(** The current token WITHOUT superseding anything — an observer's read
    (the poller must not cancel user-initiated runs). *)
val observe : t -> int Effect.t

(** Run [eff] only if [token] is still the newest — the write-time
    guard; read at effect time, once per write. *)
val when_current : t -> int -> unit Effect.t -> unit Effect.t

(** [start] + fetch + one [when_current]-guarded commit. *)
val run
  :  ?on_start:(unit -> unit)
  -> t
  -> fetch:(unit -> 'a Async.Deferred.t)
  -> commit:('a -> unit Effect.t)
  -> unit Effect.t

(** Supersede any in-flight attempt without starting a new one. *)
val invalidate : t -> unit Effect.t

(** The raw generation as a plain read, for composing a currency check
    into a caller's own atomic thunk (the poller's signature CAS).
    Prefer [observe]/[when_current] everywhere else. *)
val peek : t -> int
