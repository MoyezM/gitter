open! Core
module Query = Gitter.Search.Query
module Prompt = Gitter.Search.Prompt
module History = Gitter.Search.History
module Border = Gitter.Search.Border

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* ---- Grammar: fuzzy subsequence ---- *)

let q s = Query.parse s
let matches s candidate = Query.matches (q s) candidate

let () =
  check "subsequence matches in order" (matches "docml" "document.ml");
  check "subsequence respects order" (not (matches "mldoc" "document.ml"));
  check "full string matches itself" (matches "diff" "diff");
  check "missing char fails" (not (matches "diffz" "diff"));
  check "empty query matches everything" (matches "" "anything");
  check "empty query is empty" (Query.is_empty (q ""));
  check "spaces-only query is empty" (Query.is_empty (q "   "))
;;

(* ---- Grammar: terms all hold; negation ---- *)

let () =
  check "two terms both hold" (matches "doc ml" "document.ml");
  check "one failing term fails the row" (not (matches "doc zz" "document.ml"));
  check "negation excludes" (not (matches "diff !test" "diff_test.ml"));
  check "negation passes when absent" (matches "diff !zzz" "diff.ml");
  check "pure negative query" (matches "!mli" "document.xyz");
  check "pure negative query excludes" (not (matches "!mli" "document.mli"));
  (* Negation is subsequence too: m-l-i scattered still rejects. *)
  check "negative term is fuzzy" (not (matches "!mli" "camel.io"));
  check "lone ! is ignored (mid-typing)" (matches "!" "anything");
  check "lone ! among terms is ignored" (matches "doc !" "document.ml");
  check "! mid-term is literal" (matches "a!b" "a!b.txt");
  check "! mid-term is required" (not (matches "a!b" "ab.txt"))
;;

(* ---- Grammar: per-term smart-case ---- *)

let () =
  check "lowercase term is case-insensitive" (matches "readme" "README.md");
  check "uppercase term is case-sensitive" (matches "README" "README.md");
  check "uppercase term rejects lowercase" (not (matches "README" "readme.md"));
  (* Per-term: one sensitive, one insensitive, in one query. *)
  check "smart-case is per-term" (matches "R doc" "Read_DOC.md");
  check "negated terms smart-case too" (matches "!M" "m.txt");
  check "negated uppercase excludes exact" (not (matches "!M" "M.txt"))
;;

(* ---- Spans / positions: the underline channel ---- *)

let () =
  check
    "positions are the matched chars"
    ([%equal: int list option] (Query.positions (q "dml") "doc.ml") (Some [ 0; 4; 5 ]));
  check
    "consecutive positions merge into one span"
    ([%equal: (int * int) list option] (Query.spans (q "docml") "docml") (Some [ 0, 5 ]));
  check
    "gapped positions split spans"
    ([%equal: (int * int) list option] (Query.spans (q "dml") "doc.ml") (Some [ 0, 1; 4, 6 ]));
  check "no match, no spans" (Option.is_none (Query.spans (q "zz") "doc.ml"));
  check
    "negated terms contribute no spans"
    ([%equal: (int * int) list option] (Query.spans (q "doc !zz") "doc.ml") (Some [ 0, 3 ]));
  check
    "pure negative match has empty spans"
    ([%equal: (int * int) list option] (Query.spans (q "!zz") "doc.ml") (Some []));
  check
    "empty query has empty spans"
    ([%equal: (int * int) list option] (Query.spans (q "") "doc.ml") (Some []));
  (* Overlapping terms dedup: both terms match the same 'd'. *)
  check
    "duplicate positions dedup"
    ([%equal: int list option] (Query.positions (q "d d") "doc") (Some [ 0 ]))
;;

(* ---- Prompt lifecycle ---- *)

let apply = Prompt.apply

let type_string t s =
  String.fold s ~init:t ~f:(fun t c -> apply t (Prompt.Type (Char.to_string c)))
;;

let () =
  History.clear ();
  let t = Prompt.idle in
  check "idle is not active" (not (Prompt.is_active t));
  check "idle has no query" (Option.is_none (Prompt.query t));
  let t = apply t Prompt.Open in
  check "open activates" (Prompt.is_active t);
  check "active empty query is Some \"\"" ([%equal: string option] (Prompt.query t) (Some ""));
  let t = type_string t "parse" in
  check "typing accumulates" ([%equal: string option] (Prompt.query t) (Some "parse"));
  let t = apply t Prompt.Backspace in
  check "backspace deletes" ([%equal: string option] (Prompt.query t) (Some "pars"));
  let t = apply t Prompt.Commit in
  check "commit deactivates" (not (Prompt.is_active t));
  check "commit sets the register" ([%equal: string option] t.register (Some "pars"));
  check "committed query is the register" ([%equal: string option] (Prompt.query t) (Some "pars"));
  (* Reopen: ghost; Enter on the empty prompt re-commits (L2). *)
  let t = apply t Prompt.Open in
  check "reopen keeps the register" ([%equal: string option] t.register (Some "pars"));
  check "reopened prompt matches everything" ([%equal: string option] (Prompt.query t) (Some ""));
  let t = apply t Prompt.Commit in
  check "empty commit re-commits the ghost" ([%equal: string option] t.register (Some "pars"));
  (* Cancel restores the prior register. *)
  let t = apply t Prompt.Open in
  let t = type_string t "other" in
  let t = apply t Prompt.Cancel in
  check "cancel closes" (not (Prompt.is_active t));
  check "cancel keeps the prior register" ([%equal: string option] t.register (Some "pars"));
  (* Esc from committed clears. *)
  let t = apply t Prompt.Clear in
  check "clear drops the register" ([%equal: string option] t.register None);
  (* Implicit commit: typed text commits, but an empty prompt only closes. *)
  History.clear ();
  let t = apply Prompt.idle Prompt.Open in
  let t = type_string t "abc" in
  let t = apply t Prompt.Implicit_commit in
  check "implicit commit commits typed text" ([%equal: string option] t.register (Some "abc"));
  let t = apply t Prompt.Open in
  let t = apply t Prompt.Implicit_commit in
  check "implicit commit on empty prompt closes" (not (Prompt.is_active t));
  check
    "implicit commit on empty prompt keeps register"
    ([%equal: string option] t.register (Some "abc"));
  check
    "implicit empty commit does not push history"
    ([%equal: string option] (History.recall ~prefix:"" 1) None)
;;

(* ---- Multibyte input: Uchar printables append, backspace deletes one
   codepoint ---- *)

let () =
  let t = apply Prompt.idle Prompt.Open in
  let t = apply t (Prompt.Type "caf") in
  let t = apply t (Prompt.Type "\u{00E9}") in
  check "utf-8 input appends" ([%equal: string option] (Prompt.query t) (Some "caf\u{00E9}"));
  let t = apply t Prompt.Backspace in
  check "backspace deletes the whole codepoint"
    ([%equal: string option] (Prompt.query t) (Some "caf"));
  check
    "utf8 encodes a non-ascii scalar"
    (String.equal (Prompt.utf8 (Uchar.of_scalar_exn 0xE9)) "\u{00E9}")
;;

(* ---- History: one stack, head-dedup, prefix recall ---- *)

let () =
  History.clear ();
  History.push "parse";
  History.push "parse";
  check "head dedup" ([%equal: string option] (History.recall ~prefix:"" 1) None);
  History.push "diff !test";
  History.push "parse";
  check "newest first" ([%equal: string option] (History.recall ~prefix:"" 0) (Some "parse"));
  check
    "prefix filters the walk"
    ([%equal: string option] (History.recall ~prefix:"di" 0) (Some "diff !test"));
  check "prefix miss" ([%equal: string option] (History.recall ~prefix:"zz" 0) None);
  (* Ctrl-p/Ctrl-n walk through the prompt. *)
  let t = apply Prompt.idle Prompt.Open in
  let t = type_string t "pa" in
  let t = apply t Prompt.Recall_prev in
  check "recall finds by prefix" ([%equal: string option] (Prompt.query t) (Some "parse"));
  let t = apply t Prompt.Recall_next in
  check "recall-next returns to the typed prefix" ([%equal: string option] (Prompt.query t) (Some "pa"));
  (* Editing a recalled entry exits the walk; the edit is the new prefix. *)
  let t = apply t Prompt.Recall_prev in
  let t = apply t Prompt.Backspace in
  check "edit exits the walk" (Option.is_none t.recall);
  (* Commit during recall pushes the recalled text. *)
  History.clear ();
  History.push "alpha";
  let t = apply Prompt.idle Prompt.Open in
  let t = apply t Prompt.Recall_prev in
  let t = apply t Prompt.Commit in
  check "recalled text commits" ([%equal: string option] t.register (Some "alpha"))
;;

(* ---- Border line: modes and degradation ---- *)

let () =
  let width = 40 in
  let active = { Prompt.idle with typed = Some "abc" } in
  let committed = { Prompt.idle with register = Some "abc" } in
  check "idle renders nothing" (Option.is_none (Border.view ~prompt:Prompt.idle ~counts:None ~width));
  check "active renders" (Option.is_some (Border.view ~prompt:active ~counts:None ~width));
  check "committed renders" (Option.is_some (Border.view ~prompt:committed ~counts:None ~width));
  let fits prompt counts width =
    match Border.view ~prompt ~counts ~width with
    | None -> true
    | Some v -> Bonsai_term.View.width v <= width
  in
  check "counts fit the width" (fits active (Some "2/14") width);
  (* R2: a long query in a narrow pane truncates instead of overflowing;
     the prompt never dies. *)
  let long = { Prompt.idle with typed = Some (String.make 100 'x') } in
  check "narrow pane truncates the query" (fits long (Some "12/345") 12);
  check "tiny pane still renders" (Option.is_some (Border.view ~prompt:long ~counts:None ~width:4))
;;

let () = print_endline "All search tests passed."
