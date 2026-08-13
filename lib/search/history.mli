open! Core

(** ONE global stack of committed queries (H1) — shared across panes
    (search [parse] in the tree, then want it in the diff), newest
    first, deduplicated against the head. In-memory for v1; persisting
    across sessions is explicitly deferred (H4). *)

val push : string -> unit

(** The [n]th newest entry starting with [prefix] (0-based) — the
    prefix-filtered recall walk Ctrl-p/Ctrl-n make. *)
val recall : prefix:string -> int -> string option

(** Tests only. *)
val clear : unit -> unit
