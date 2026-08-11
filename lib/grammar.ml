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

type injection_rule =
  { block : string
  ; info : string
  ; content : string
  }

(* Markdown is the only language here that embeds others. Kept as data so
   [Highlight] never names a grammar node: it walks for [block], reads
   [info] for the language, and highlights [content] with it. *)
let injection_rule ext =
  match ext with
  | "md" | "markdown" ->
    Some
      { block = "fenced_code_block"; info = "info_string"; content = "code_fence_content" }
  | _ -> None
;;

(* A fence says "```python"; the table is keyed by EXTENSION ("py"). Only the
   names that actually differ need an entry — "bash", "go", "css", "toml" and
   friends are already extensions and fall through. *)
let extension_of_language = function
  | "python" -> "py"
  | "rust" -> "rs"
  | "javascript" | "node" | "es6" -> "js"
  | "typescript" -> "ts"
  | "ocaml" -> "ml"
  | "shell" | "console" | "shell-session" -> "sh"
  | "golang" -> "go"
  | "c++" | "cplusplus" -> "cpp"
  | "yml" -> "yaml"
  | "markdown" -> "md"
  | other -> other
;;

let find_embedded info =
  (* "```python title=x" — the language is the first token. Fences are also
     commonly capitalised ("```Python"). *)
  match
    String.strip info |> String.split_on_chars ~on:[ ' '; '\t'; ','; '{' ] |> List.hd
  with
  | None -> None
  | Some "" -> None
  | Some name ->
    let ext = extension_of_language (String.lowercase name) in
    Option.map (find ext) ~f:(fun spec -> ext, spec)
;;
