(** Extension -> highlighting spec: the opam-shipped builtin grammars plus
    every vendored grammar from [grammars/registry]. The only language table
    in the codebase — [Highlight] holds zero language knowledge. *)

val find : string -> Grammar_registry.spec option
