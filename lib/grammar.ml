open! Core

(* The one language table. Builtins ship inside the tree-sitter opam
   package; everything else comes from the generated registry — adding a
   vendored language (scripts/add-grammar) never touches this file. *)

let builtin ext : Grammar_registry.spec option =
  match ext with
  | "ml" ->
    Some
      { Grammar_registry.language = Tree_sitter_ocaml.ocaml
      ; highlights_query = Tree_sitter_ocaml.ocaml_highlights_query
      }
  | "mli" ->
    Some
      { Grammar_registry.language = Tree_sitter_ocaml.interface
      ; highlights_query = Tree_sitter_ocaml.interface_highlights_query
      }
  | "json" ->
    Some
      { Grammar_registry.language = Tree_sitter_json.language
      ; highlights_query = Tree_sitter_json.highlights_query
      }
  | _ -> None
;;

let find ext = Option.first_some (builtin ext) (Grammar_registry.find ext)
