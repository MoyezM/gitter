open! Core
module H = Gitter.Highlight

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* Prefix match: upstream queries use dotted names ("keyword.function"),
   and the theme groups by the same prefix. *)
let has ~capture spans =
  List.exists spans ~f:(fun (s : H.Span.t) -> String.is_prefix s.capture ~prefix:capture)
;;

(* OCaml, via the bundled grammar. *)
let () =
  let src = "let x = 42\n(* a\nb *)\nlet s = \"hi\"\n" in
  let t = H.of_content ~path:"foo.ml" src in
  let l1 = H.line t 1 in
  check
    "keyword 'let' at line 1, cols 0-3"
    (List.exists l1 ~f:(fun (s : H.Span.t) ->
       String.equal s.capture "keyword" && s.start_col = 0 && s.end_col = 3));
  check "number on line 1" (has ~capture:"number" l1);
  (* The multi-line comment must be sliced across both lines. *)
  check "comment slice on line 2" (has ~capture:"comment" (H.line t 2));
  check "comment slice on line 3" (has ~capture:"comment" (H.line t 3));
  check "string on line 4" (has ~capture:"string" (H.line t 4));
  check "unsupported extension yields no spans" (Array.length (H.of_content ~path:"x.txt" src) = 0)
;;

(* Python, via our vendored grammar. The direct call comes first so a broken
   query surfaces its real exception instead of silently-empty spans. *)
let () =
  let py = "def foo():\n    return True\n" in
  let (_ : (int * int * string) list) =
    (Option.value_exn (Grammar_registry.find "py")) py
  in
  let t = H.of_content ~path:"a.py" py in
  check "py: 'def' keyword" (has ~capture:"keyword" (H.line t 1));
  check "py: function name" (has ~capture:"function" (H.line t 1));
  check "py: 'return' keyword" (has ~capture:"keyword" (H.line t 2))
;;

(* JavaScript, via our vendored grammar. *)
let () =
  let js = "function foo() {\n  return 42;\n}\n" in
  let t = H.of_content ~path:"a.js" js in
  check "js: 'function' keyword" (has ~capture:"keyword" (H.line t 1));
  check "js: function name" (has ~capture:"function" (H.line t 1));
  check "js: number" (has ~capture:"number" (H.line t 2));
  check "js: jsx extension routes too" (Array.length (H.of_content ~path:"a.jsx" js) > 0)
;;

(* Every registered grammar must have a loadable query: queries are lazy, so
   force each one. This is what makes scripts/add-grammar's build+test gate a
   real validation for languages without dedicated tests above. *)
let () =
  List.iter Grammar_registry.extensions ~f:(fun ext ->
    let highlight = Option.value_exn (Grammar_registry.find ext) in
    let (_ : (int * int * string) list) = highlight "x\n" in
    ())
;;

(* C, via our vendored grammar. *)
let () =
  let c = "int main() {\n  return 0;\n}\n" in
  let t = H.of_content ~path:"a.c" c in
  check "c: type (int)" (has ~capture:"type" (H.line t 1));
  check "c: 'return' keyword" (has ~capture:"keyword" (H.line t 2))
;;

let () = print_endline "All highlight tests passed."
