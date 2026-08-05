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
  check "markdown: session exists" (not (H.is_empty t))
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

let () = print_endline "All highlight tests passed."
