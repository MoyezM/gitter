(** Session-based syntax highlighting, helix-style: parse a file ONCE into a
    tree-sitter tree (in a separate domain — the C stubs hold the runtime
    lock), then each frame run the highlight query over only the lines being
    rendered. Failure at any stage means "no highlighting", never an
    exception. Channel convention: syntax owns the foreground; diff status
    owns the background. *)

module Span : sig
  type t =
    { start_col : int (** byte column within the line, inclusive *)
    ; end_col : int (** byte column within the line, exclusive *)
    ; capture : string (** highlight capture name, e.g. "keyword.function" *)
    }
end

type t

val empty : t

(** No session — unknown language, failed parse, or [empty]. *)
val is_empty : t -> bool

(** Parse on the calling domain. For tests and benchmarks; the app uses
    [create]. *)
val create_sync : path:string -> string -> t

(** Parse in a separate domain via [Cpu.in_domain]; rendering never blocks
    on it. Falls back to [empty] on any failure (including domain
    exhaustion). *)
val create : path:string -> string -> t Async.Deferred.t

module Window : sig
  (** Spans for a contiguous line range, indexed by ABSOLUTE 1-based line
      number — the window carries its own origin, so callers cannot desync
      from the range it was built over. Out-of-range lines yield []. *)
  type t

  val line : t -> int -> Span.t list
end

(** One ranged query covering lines [from_line..to_line] (1-based,
    inclusive). Spans within each line are sorted by [start_col]. *)
val window : t -> from_line:int -> to_line:int -> Window.t
