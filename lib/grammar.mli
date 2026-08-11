(** Extension -> highlighting spec: the opam-shipped builtin grammars plus
    every vendored grammar from [grammars/registry]. The only language table
    in the codebase — [Highlight] holds zero language knowledge. *)

val find : string -> Grammar_registry.spec option

(** How a language embeds other languages, as DATA so that [Highlight] can
    act on it without naming a grammar: [block] is the container node kind,
    [info] the child whose text names the embedded language, and [content]
    the child holding the code to highlight. *)
type injection_rule =
  { block : string
  ; info : string
  ; content : string
  }

(** The injection rule for a file extension, if it has one. *)
val injection_rule : string -> injection_rule option

(** Resolve the text of an [info] node (a markdown fence's "python", say) to
    the extension it normalises to and its spec. Handles the
    language-name-vs-extension mismatch and any trailing attributes. None when
    the language is unknown, unsupported, or absent.

    The extension comes back because it is the only usable cache key for a
    grammar: [Tree_sitter.Language.name] is "" for every vendored grammar, so
    keying a query cache on it silently collapses every language into one. *)
val find_embedded : string -> (string * Grammar_registry.spec) option
