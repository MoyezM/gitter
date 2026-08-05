open! Core
module H = Gitter.Highlight

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* Session + single-line window, as the diff pane uses it. *)
let session ~path content = H.create_sync ~path content

let line t n =
  H.line_in_window (H.window t ~from_line:n ~to_line:n) ~from_line:n n
;;

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
  check "window: comment at 2" (has ~capture:"comment" (H.line_in_window w ~from_line:2 2));
  check "window: string at 4" (has ~capture:"string" (H.line_in_window w ~from_line:2 4));
  check "unsupported extension yields no session" (Option.is_none (session ~path:"x.txt" src))
;;

(* Python, via our vendored grammar. *)
let () =
  let py = "def foo():\n    return True\n" in
  let t = session ~path:"a.py" py in
  check "py: session exists" (Option.is_some t);
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
  check "js: jsx extension routes too" (Option.is_some (session ~path:"a.jsx" js))
;;

(* C, via our vendored grammar. *)
let () =
  let c = "int main() {\n  return 0;\n}\n" in
  let t = session ~path:"a.c" c in
  check "c: type (int)" (has ~capture:"type" (line t 1));
  check "c: 'return' keyword" (has ~capture:"keyword" (line t 2))
;;

(* Every registered grammar must have a loadable query: force each one so
   scripts/add-grammar's build+test gate validates languages without
   dedicated tests above. *)
let () =
  List.iter Grammar_registry.extensions ~f:(fun ext ->
    match Grammar_registry.find ext with
    | None -> failwithf "registry advertises %s but find returned None" ext ()
    | Some (language, source) ->
      let (_ : Tree_sitter.Query.t) = Tree_sitter.Query.create (language ()) ~source in
      ())
;;

let () = print_endline "All highlight tests passed."
