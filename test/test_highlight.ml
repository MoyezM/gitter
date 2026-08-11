open! Core
module H = Gitter.Highlight

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* Session + single-line window, as the diff pane uses it. *)
let session ~path content = H.create_sync ~path content

let line t n = H.Window.line (H.window t ~from_line:n ~to_line:n) n

(* Prefix match: upstream queries use dotted names ("keyword.function"),
   and the theme groups by the same prefix. *)
let has ~capture spans =
  List.exists spans ~f:(fun (s : H.Span.t) -> String.is_prefix s.capture ~prefix:capture)
;;

(* OCaml, via the bundled grammar. *)
let () =
  let src = "let x = 42\n(* a\nb *)\nlet s = \"hi\"\n" in
  let t = session ~path:"foo.ml" src in
  let l1 = line t 1 in
  check
    "keyword 'let' at line 1, cols 0-3"
    (List.exists l1 ~f:(fun (s : H.Span.t) ->
       String.is_prefix s.capture ~prefix:"keyword" && s.start_col = 0 && s.end_col = 3));
  check "number on line 1" (has ~capture:"number" l1);
  (* The multi-line comment must be sliced across both lines. *)
  check "comment slice on line 2" (has ~capture:"comment" (line t 2));
  check "comment slice on line 3" (has ~capture:"comment" (line t 3));
  check "string on line 4" (has ~capture:"string" (line t 4));
  (* A multi-line window indexes correctly. *)
  let w = H.window t ~from_line:2 ~to_line:4 in
  check "window: comment at 2" (has ~capture:"comment" (H.Window.line w 2));
  check "window: string at 4" (has ~capture:"string" (H.Window.line w 4));
  check "unsupported extension yields no session" (H.is_empty (session ~path:"x.txt" src))
;;

(* Python, via our vendored grammar. *)
let () =
  let py = "def foo():\n    return True\n" in
  let t = session ~path:"a.py" py in
  check "py: session exists" (not (H.is_empty t));
  check "py: 'def' keyword" (has ~capture:"keyword" (line t 1));
  check "py: function name" (has ~capture:"function" (line t 1));
  check "py: 'return' keyword" (has ~capture:"keyword" (line t 2))
;;

(* JavaScript, via our vendored grammar. *)
let () =
  let js = "function foo() {\n  return 42;\n}\n" in
  let t = session ~path:"a.js" js in
  check "js: 'function' keyword" (has ~capture:"keyword" (line t 1));
  check "js: function name" (has ~capture:"function" (line t 1));
  check "js: number" (has ~capture:"number" (line t 2));
  check "js: jsx extension routes too" (not (H.is_empty (session ~path:"a.jsx" js)))
;;

(* C, via our vendored grammar. *)
let () =
  let c = "int main() {\n  return 0;\n}\n" in
  let t = session ~path:"a.c" c in
  check "c: type (int)" (has ~capture:"type" (line t 1));
  check "c: 'return' keyword" (has ~capture:"keyword" (line t 2))
;;

(* The 2026-08 batch: one shallow assertion each — the registry loop below
   already validates every grammar's query compiles. *)
let () =
  let t = session ~path:"a.ts" "const x: number = 42\n" in
  check "ts: keyword" (has ~capture:"keyword" (line t 1));
  let t = session ~path:"a.tsx" "const el = <div className=\"x\" />\n" in
  check "tsx: session exists" (not (H.is_empty t));
  let t = session ~path:"a.html" "<body class=\"main\"></body>\n" in
  check "html: tag" (has ~capture:"tag" (line t 1) || has ~capture:"constructor" (line t 1));
  let t = session ~path:"a.css" ".cls { color: red; }\n" in
  check "css: session exists" (not (H.is_empty t));
  let t = session ~path:"a.go" "func main() {\n\treturn\n}\n" in
  check "go: keyword" (has ~capture:"keyword" (line t 1));
  let t = session ~path:"a.rs" "fn main() { let x = 1; }\n" in
  check "rust: keyword" (has ~capture:"keyword" (line t 1));
  let t = session ~path:"a.sh" "if true; then\n  echo hi\nfi\n" in
  check "bash: keyword" (has ~capture:"keyword" (line t 1));
  let t = session ~path:"a.yaml" "key: value\n" in
  check "yaml: session exists" (not (H.is_empty t));
  let t = session ~path:"a.toml" "[section]\nkey = 1\n" in
  check "toml: session exists" (not (H.is_empty t));
  let t = session ~path:"a.md" "# Title\n" in
  check "markdown: session exists" (not (H.is_empty t));
  let t = session ~path:"a.nix" "let x = 1; in { inherit x; }\n" in
  check "nix: keyword" (has ~capture:"keyword" (line t 1));
  let t = session ~path:"gitter.opam" "opam-version: \"2.0\"\ndepends: [ \"dune\" ]\n" in
  check "opam: session exists" (not (H.is_empty t))
;;

(* Every registered grammar must have a loadable query: force each one so
   scripts/add-grammar's build+test gate validates languages without
   dedicated tests above. *)
let () =
  List.iter Grammar_registry.extensions ~f:(fun ext ->
    match Grammar_registry.find ext with
    | None -> failwithf "registry advertises %s but find returned None" ext ()
    | Some { Grammar_registry.language; highlights_query } ->
      let (_ : Tree_sitter.Query.t) =
        Tree_sitter.Query.create (language ()) ~source:highlights_query
      in
      ())
;;

(* --- markdown code-block injection ---------------------------------------
   A fenced block with a language gets that language's grammar. The parent
   markdown captures the whole block, so the check that matters is that the
   BODY is owned by the embedded grammar, not that some span exists. *)
let () =
  let md =
    String.concat_lines
      [ "# Title"
      ; ""
      ; "```python"
      ; "def foo():"
      ; "    return True"
      ; "```"
      ; ""
      ; "```"
      ; "def foo():"
      ; "```"
      ]
  in
  let t = session ~path:"notes.md" md in
  check "md: session exists" (not (H.is_empty t));
  (* line 4 is "def foo():" inside the python fence *)
  check "md: python keyword inside the fence" (has ~capture:"keyword" (line t 4));
  check "md: python function name inside the fence" (has ~capture:"function" (line t 4));
  (* line 5 "    return True" — a keyword the markdown grammar cannot produce *)
  check "md: python keyword on the fence's second line" (has ~capture:"keyword" (line t 5));
  (* An UNLABELLED fence must stay markdown-only: same text, line 9. *)
  check
    "md: unlabelled fence gets no python"
    (not (has ~capture:"keyword" (line t 9)));
  (* Prose outside the fence is still markdown. *)
  check "md: heading still highlighted" (not (List.is_empty (line t 1)));
  (* Clipping: markdown tags the whole fenced_code_block @text.literal, and
     the renderer awards an overlap to whichever span starts first. If that
     capture were left spanning the body, it would start at column 0 and
     swallow every python span behind it. It must be gone from the body... *)
  check
    "md: parent block capture is clipped out of the body"
    (not (has ~capture:"text.literal" (line t 4)));
  (* ...but NOT from the fence line itself, which markdown still owns. *)
  check "md: fence line keeps markdown styling" (not (List.is_empty (line t 3)))
;;

(* TWO different languages in one document, checked against ground truth.
   "some keyword capture exists" is too weak to be worth writing: a block
   highlighted with the WRONG language's query still produces keywords, at
   valid positions, with meaningless names. The invariant that actually holds
   is that a fenced block highlights exactly as the same code would in a file
   of that language — which also proves the parent's block capture was
   clipped out of the body. *)
let () =
  let rs = "pub fn evict(&mut self) -> usize {\n    let n = 1;\n    n\n}\n" in
  let md =
    String.concat_lines
      ([ "```python"; "def foo():"; "    return 1"; "```"; ""; "```rust" ]
       @ String.split_lines rs
       @ [ "```" ])
  in
  let injected = session ~path:"notes.md" md in
  let standalone = session ~path:"x.rs" rs in
  let caps t n =
    line t n
    |> List.map ~f:(fun (s : H.Span.t) -> s.start_col, s.end_col, s.capture)
    |> List.sort ~compare:Poly.compare
  in
  (* The rust body starts at md line 7 and at standalone line 1. *)
  List.iteri (String.split_lines rs) ~f:(fun i _ ->
    check
      (sprintf "md: rust body line %d identical to standalone" (i + 1))
      ([%equal: (int * int * string) list] (caps injected (7 + i)) (caps standalone (1 + i))))
;;

(* Shapes real documents actually contain: a fence carrying attributes after
   the language, and a fence indented inside a list item (where the grammar
   interleaves block_continuation nodes through the content). *)
let () =
  let md =
    String.concat_lines
      [ "```python title=\"example.py\""
      ; "def foo():"
      ; "```"
      ; ""
      ; "- a list item:"
      ; ""
      ; "  ```rust"
      ; "  fn main() {}"
      ; "  ```"
      ]
  in
  let t = session ~path:"notes.md" md in
  check "md: fence with attributes still resolves" (has ~capture:"keyword" (line t 2));
  check "md: fence inside a list item resolves" (has ~capture:"keyword" (line t 8))
;;

(* Layers nest. A markdown block inside markdown is just another layer, so
   the python inside IT highlights too — depth is not a special case. *)
let () =
  let md =
    String.concat_lines
      [ "````markdown"
      ; "# Inner"
      ; ""
      ; "```python"
      ; "def deep():"
      ; "    return 1"
      ; "```"
      ; "````"
      ]
  in
  let t = session ~path:"notes.md" md in
  check "md: python nested two layers deep" (has ~capture:"keyword" (line t 5))
;;

(* An unknown fence language must not break the file — the rest still
   highlights, and the block is left to markdown. *)
let () =
  let md =
    String.concat_lines
      [ "```nosuchlang"; "def foo():"; "```"; ""; "```py"; "def bar():"; "```" ]
  in
  let t = session ~path:"notes.md" md in
  check "md: unknown fence language is inert" (not (has ~capture:"keyword" (line t 2)));
  (* "py" is the extension spelling and must work as well as "python". *)
  check "md: extension-spelled fence works" (has ~capture:"keyword" (line t 6))
;;

let () = print_endline "All highlight tests passed."
