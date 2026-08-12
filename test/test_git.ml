open! Core
module Status = Gitter.Git.Status
module Diff = Gitter.Git.Diff
module Branch_stack = Gitter.Git.Branch_stack

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* --- status --porcelain=v2 ---------------------------------------------- *)

let porcelain_sample =
  String.concat_lines
    [ "# branch.oid 3fa1b2c"
    ; "# branch.head feat/sessions"
    ; "1 .M N... 100644 100644 100644 abc1 abc2 lib/app.ml"
    ; "1 M. N... 100644 100644 100644 abc1 abc2 bin/main.ml"
    ; "1 MM N... 100644 100644 100644 abc1 abc2 lib/both.ml"
    ; "2 R. N... 100644 100644 100644 abc1 abc2 R100 lib/new_name.ml\tlib/old_name.ml"
    ; "u UU N... 100644 100644 100644 100644 a b c lib/conflict.ml"
    ; "? test/scratch.ml"
    ; "1 .M N... 100644 100644 100644 abc1 abc2 docs/with space.md"
    ; "1 .M N... 100644 100644 100644 abc1 abc2 \"h\\303\\251llo.ml\""
    ]
;;

let entries = Status.parse porcelain_sample

let find path =
  List.find_exn entries ~f:(fun (e : Status.Entry.t) -> String.equal e.path path)
;;

let () =
  check "entry count (headers skipped)" (List.length entries = 8);
  (* core.quotePath: non-ASCII paths arrive C-quoted with octal escapes and
     must be unquoted to the real bytes, or every git command on them fails. *)
  check
    "quoted path unquoted"
    (List.exists entries ~f:(fun e -> String.equal e.path "h\195\169llo.ml"));
  let unstaged = find "lib/app.ml" in
  check "unstaged: XY chars" (Char.equal unstaged.index '.' && Char.equal unstaged.worktree 'M');
  let staged = find "bin/main.ml" in
  check "staged: XY chars" (Char.equal staged.index 'M' && Char.equal staged.worktree '.');
  let both = find "lib/both.ml" in
  check "both sides: XY chars" (Char.equal both.index 'M' && Char.equal both.worktree 'M');
  let renamed = find "lib/new_name.ml" in
  check
    "rename carries origin"
    (Status.Entry.Kind.equal renamed.kind (Renamed { from = "lib/old_name.ml" }));
  check "rename: index char" (Char.equal renamed.index 'R');
  let conflict = find "lib/conflict.ml" in
  check "unmerged kind" (Status.Entry.Kind.equal conflict.kind Unmerged);
  let untracked = find "test/scratch.ml" in
  check "untracked kind" (Status.Entry.Kind.equal untracked.kind Untracked);
  check "untracked: XY chars" (Char.equal untracked.index '?' && Char.equal untracked.worktree '?');
  check "path with space survives" (String.equal (find "docs/with space.md").path "docs/with space.md")
;;

(* Hunk.raw must be the VERBATIM bytes — staging reconstructs `git apply`
   patches from it, so it round-trips exactly, including the no-newline
   marker the parsed lines drop. *)
let () =
  let hunk_text = "@@ -1,2 +1,2 @@\n context\n-old\n+new\n\\ No newline at end of file" in
  let sample =
    String.concat_lines [ "diff --git a/x.ml b/x.ml"; "--- a/x.ml"; "+++ b/x.ml" ]
    ^ hunk_text
    ^ "\n"
  in
  match Diff.parse sample with
  | [ { hunks = [ h ]; _ } ] ->
    check "raw round-trips verbatim" (String.equal h.raw (hunk_text ^ "\n"));
    check "no-newline marker not parsed as a line" (List.length h.lines = 3);
    check
      "patch embeds raw byte-exactly"
      (String.is_suffix (Gitter.Git.Stage.patch ~path:"x.ml" ~raw:h.raw) ~suffix:h.raw)
  | _ -> check "raw sample parses" false
;;

(* C-quoted path decoding: named control escapes, octal, and totality on
   malformed input (git never emits it, but the parser must not raise). *)
let () =
  check "unquote passthrough" (String.equal (Status.unquote "plain.ml") "plain.ml");
  check "unquote octal" (String.equal (Status.unquote "\"h\\303\\251.ml\"") "h\195\169.ml");
  check
    "unquote named control escapes"
    (String.equal (Status.unquote "\"a\\ab\\bv\\vf\\f.ml\"") "a\007b\bv\011f\012.ml");
  check "unquote escaped quote" (String.equal (Status.unquote "\"a\\\"b\"") "a\"b");
  check "unquote malformed octal is total" (String.equal (Status.unquote "\"x\\7zz\"") "x7zz")
;;

(* --branch headers -> branch info. *)
let () =
  let out =
    String.concat_lines
      [ "# branch.oid abc123"; "# branch.head main"; "# branch.ab +2 -1"; "1 .M N... 1 2 3 4 5 x" ]
  in
  check
    "branch with upstream"
    (match Status.branch out with
     | Some { head = "main"; ahead = 2; behind = 1 } -> true
     | _ -> false);
  check
    "branch without upstream"
    (match Status.branch "# branch.head feat/x\n" with
     | Some { head = "feat/x"; ahead = 0; behind = 0 } -> true
     | _ -> false);
  check "no headers -> no branch" (Option.is_none (Status.branch "1 .M N... 1 2 3 4 5 x\n"))
;;

(* --- unified diff ------------------------------------------------------- *)

let diff_sample =
  String.concat_lines
    [ "diff --git a/lib/app.ml b/lib/app.ml"
    ; "index 1234567..89abcde 100644"
    ; "--- a/lib/app.ml"
    ; "+++ b/lib/app.ml"
    ; "@@ -10,6 +10,8 @@ let view ="
    ; " let x = 1 in"
    ; "-let y = 2 in"
    ; "+let y = 3 in"
    ; "+let z = 4 in"
    ; " x + y"
    ; "@@ -40,3 +42,3 @@"
    ; "-let old = ()"
    ; "+let new_ = ()"
    ; " ()"
    ; "diff --git a/README.md b/README.md"
    ; "index 111..222 100644"
    ; "--- a/README.md"
    ; "+++ b/README.md"
    ; "@@ -1,2 +1,3 @@"
    ; " # gitter"
    ; "+A git TUI."
    ]
;;

let files = Diff.parse diff_sample

let () =
  check "two files" (List.length files = 2);
  let app = List.hd_exn files in
  check "path from header" (String.equal app.path "lib/app.ml");
  check "two hunks" (List.length app.hunks = 2);
  let h1 = List.hd_exn app.hunks in
  check "hunk header" (String.is_prefix h1.header ~prefix:"@@ -10,6 +10,8 @@");
  check
    "line kinds in order"
    (List.equal
       Diff.Line.equal
       h1.lines
       [ Context "let x = 1 in"
       ; Removed "let y = 2 in"
       ; Added "let y = 3 in"
       ; Added "let z = 4 in"
       ; Context "x + y"
       ]);
  check "hunk starts parsed" (h1.old_start = 10 && h1.new_start = 10);
  check
    "numbering threads through mixed lines"
    (List.equal
       (fun (o1, n1, l1) (o2, n2, l2) ->
         [%equal: int option] o1 o2 && [%equal: int option] n1 n2 && Diff.Line.equal l1 l2)
       (Diff.Hunk.numbered h1)
       [ Some 10, Some 10, Context "let x = 1 in"
       ; Some 11, None, Removed "let y = 2 in"
       ; None, Some 11, Added "let y = 3 in"
       ; None, Some 12, Added "let z = 4 in"
       ; Some 12, Some 13, Context "x + y"
       ]);
  let readme = List.last_exn files in
  check "second file path" (String.equal readme.path "README.md");
  check
    "second file lines"
    (List.equal
       Diff.Line.equal
       (List.hd_exn readme.hunks).lines
       [ Context "# gitter"; Added "A git TUI." ])
;;


(* Hunk spans: the @@ counts, and the count-zero convention. Boundaries
   are what the diff pane measures elided runs against, so they are
   pinned against real git output rather than derived from the body. *)
let () =
  let h1 = List.hd_exn (List.hd_exn files).hunks in
  check "counts parsed" (h1.old_count = 6 && h1.new_count = 8);
  check "span ends past the last line" (Diff.Hunk.old_after h1 = 16);
  check "span starts after the line before" (Diff.Hunk.old_before h1 = 9);
  (* Omitted counts mean 1. *)
  let one =
    String.concat_lines
      [ "diff --git a/x b/x"; "--- a/x"; "+++ b/x"; "@@ -13 +13 @@"; "-a"; "+b" ]
  in
  (match Diff.parse one with
   | [ { hunks = [ h ]; _ } ] ->
     check "omitted count is 1" (h.old_count = 1 && h.new_count = 1);
     check "omitted-count boundaries" (Diff.Hunk.old_after h = 14 && Diff.Hunk.old_before h = 12)
   | _ -> check "omitted-count sample parses" false);
  (* diff.context = 0 makes EVERY hunk a count-zero hunk. git anchors a
     zero-count side AFTER the named line, which is the opposite of what
     a body-derived extent assumes; the middle gap here is old [5,11] and
     new [7,13], seven lines on both sides. *)
  let ctx0 =
    String.concat_lines
      [ "diff --git a/f.txt b/f.txt"
      ; "--- a/f.txt"
      ; "+++ b/f.txt"
      ; "@@ -4,0 +5,2 @@ c4"
      ; "+NEW1"
      ; "+NEW2"
      ; "@@ -12,2 +13,0 @@ c11"
      ; "-c12"
      ; "-c13"
      ]
  in
  (match Diff.parse ctx0 with
   | [ { hunks = [ a; b ]; _ } ] ->
     check "zero old count anchors after the line" (Diff.Hunk.old_after a = 5);
     check "nonzero new count ends past its span" (Diff.Hunk.new_after a = 7);
     check "nonzero old count starts after its predecessor" (Diff.Hunk.old_before b = 11);
     check "zero new count ends at the line" (Diff.Hunk.new_before b = 13);
     check
       "both sides measure the gap the same"
       (Diff.Hunk.old_before b - Diff.Hunk.old_after a
        = Diff.Hunk.new_before b - Diff.Hunk.new_after a);
     check "counts agree with the body" (Diff.Hunk.counts_agree a && Diff.Hunk.counts_agree b)
   | _ -> check "context-0 sample parses" false);
  (* A header that does not parse yields no hunk: anchoring it at a
     guessed line 0 would make [raw] apply somewhere else entirely. *)
  (match Diff.parse (String.concat_lines [ "diff --git a/x b/x"; "@@ junk @@"; " a" ]) with
   | [ { hunks = []; _ } ] -> check "unparseable header drops the hunk" true
   | _ -> check "unparseable header drops the hunk" false);
  (* A body short of what the header promised is detectable — that is the
     signal that disables context expansion instead of misnumbering it. *)
  (match
     Diff.parse
       (String.concat_lines [ "diff --git a/x b/x"; "@@ -1,3 +1,3 @@"; " a"; "-b"; "+c" ])
   with
   | [ { hunks = [ h ]; _ } ] -> check "short body is caught" (not (Diff.Hunk.counts_agree h))
   | _ -> check "short-body sample parses" false)
;;

(* diff.suppressBlankEmpty writes a blank context line as a ZERO-LENGTH
   line instead of a lone space. Dropping it as junk shifts every
   subsequent line number in the hunk. *)
let () =
  let sample =
    String.concat_lines
      [ "diff --git a/x b/x"; "--- a/x"; "+++ b/x"; "@@ -1,4 +1,4 @@"; " a"; ""; " c"; "-d"; "+D" ]
  in
  match Diff.parse sample with
  | [ { hunks = [ h ]; _ } ] ->
    check
      "blank context line survives"
      (List.equal Diff.Line.equal h.lines [ Context "a"; Context ""; Context "c"; Removed "d"; Added "D" ]);
    check "counts still agree" (Diff.Hunk.counts_agree h);
    check
      "numbering is not shifted by the blank"
      ([%equal: (int option * int option) list]
         (List.map (Diff.Hunk.numbered h) ~f:(fun (o, n, _) -> o, n))
         [ Some 1, Some 1; Some 2, Some 2; Some 3, Some 3; Some 4, None; None, Some 4 ])
  | _ -> check "blank-line sample parses" false
;;

(* git does not quote spaces in the "diff --git" header, so splitting on
   the last space turns a real path into a suffix of itself. *)
let () =
  let path text =
    match Diff.parse (String.concat_lines [ text; "@@ -1 +1 @@"; "-a"; "+b" ]) with
    | [ f ] -> f.path
    | _ -> "<unparsed>"
  in
  check "plain path" (String.equal (path "diff --git a/lib/app.ml b/lib/app.ml") "lib/app.ml");
  check
    "path with a space"
    (String.equal (path "diff --git a/docs/with space.md b/docs/with space.md") "docs/with space.md");
  check
    "rename keeps the b-side"
    (String.equal (path "diff --git a/old name.ml b/new name.ml") "new name.ml")
;;

(* numstat: per-file counts, rename fields resolved to the new path,
   binary files dropped. *)
let () =
  let out = "3\t1\tlib/app.ml\n-\t-\tbin/blob.png\n2\t0\tlib/{old => new}/x.ml\n5\t4\ta.ml => b.ml\n" in
  let parsed = Gitter.Git.Diff.numstat out in
  check
    "numstat parses counts, renames, and drops binary"
    ([%equal: (string * (int * int)) list]
       parsed
       [ "lib/app.ml", (3, 1); "lib/new/x.ml", (2, 0); "b.ml", (5, 4) ])
;;


(* raw diff: the committed pane's entry + mark-key source. *)
let () =
  let o40 = String.make 40 'a'
  and n40 = String.make 40 'b' in
  let out =
    String.concat
      ~sep:"\n"
      [ sprintf ":100644 100644 %s %s M\tlib/app.ml" o40 n40
      ; sprintf ":000000 100644 %s %s A\tnew.ml" (String.make 40 '0') n40
      ; sprintf ":100644 100644 %s %s R100\told.ml\tnew_name.ml" o40 n40
      ; "garbage line"
      ]
  in
  let parsed = Gitter.Git.Status.parse_raw out in
  let entries = List.map parsed ~f:fst in
  let letters = List.map entries ~f:(fun e -> e.Gitter.Git.Status.Entry.index) in
  let paths = List.map entries ~f:(fun e -> e.Gitter.Git.Status.Entry.path) in
  check "raw letters" ([%equal: char list] letters [ 'M'; 'A'; 'R' ]);
  check "raw paths (rename keeps new)"
    ([%equal: string list] paths [ "lib/app.ml"; "new.ml"; "new_name.ml" ]);
  check "raw blob pairs"
    ([%equal: (string * string) list] (List.map parsed ~f:snd) [ o40, n40; String.make 40 '0', n40; o40, n40 ]);
  check "rename carries from"
    (List.exists entries ~f:(fun e ->
       match e.kind with
       | Gitter.Git.Status.Entry.Kind.Renamed { from } -> String.equal from "old.ml"
       | _ -> false))
;;

(* --- branch stack ------------------------------------------------------- *)

(* Two commit lines above the base: c -> a -> p -> m and b -> q -> m. *)
let stack_sha i = sprintf "%040d" i
let m, p, a, q, b, c = ( stack_sha 0, stack_sha 1, stack_sha 2
                       , stack_sha 3, stack_sha 4, stack_sha 5 )

let stack_dag =
  String.concat
    ~sep:"\n"
    [ sprintf "%s %s" c a
    ; sprintf "%s %s" a p
    ; sprintf "%s %s" p m
    ; sprintf "%s %s" b q
    ; sprintf "%s %s" q m
    ; m
    ]
;;

let stack_heads ~with_ccc =
  String.concat
    ~sep:"\n"
    ([ sprintf "main\t%s" m; sprintf "aaa\t%s" a; sprintf "bbb\t%s" b ]
     @ if with_ccc then [ sprintf "ccc\t%s" c ] else [])
;;

(* aaa once sat at q (inside bbb's history) and bbb at p (inside aaa's) —
   what a couple of rebases inside a stack leave behind. Matching at reflog
   tips then names each branch the other's parent. *)
let crossed_reflogs =
  String.concat ~sep:"\n" [ sprintf "aaa\t%s" q; sprintf "bbb\t%s" p ]
;;

let stack ~with_ccc ~reflogs =
  Branch_stack.parse
    ~heads:(stack_heads ~with_ccc)
    ~dag:stack_dag
    ~reflogs
    ~trunk:"main"
    ~current:None
  |> List.map ~f:(fun br -> br.Branch_stack.Branch.name, br.Branch_stack.Branch.depth)
;;

(* REGRESSION: the cycle used to make the sibling sort recurse until the
   stack died — subtree_has_current/subtree_rank walk children and, unlike
   [walk], carry no visited set. It only fires when some node has TWO
   children (List.sort never calls the comparator on a singleton), hence
   the ccc case. With one child it terminated but dropped the whole cycle
   from the display instead. Both must now match the uncrossed tree. *)
let () =
  let expected = [ "main", 0; "aaa", 1; "ccc", 2; "bbb", 1 ] in
  check
    "stack: plain reflogs give the trunk-rooted tree"
    ([%equal: (string * int) list] (stack ~with_ccc:true ~reflogs:"") expected);
  check
    "stack: parent cycle is cut, sibling sort terminates"
    ([%equal: (string * int) list] (stack ~with_ccc:true ~reflogs:crossed_reflogs) expected);
  check
    "stack: parent cycle does not swallow its branches"
    ([%equal: (string * int) list]
       (stack ~with_ccc:false ~reflogs:crossed_reflogs)
       [ "main", 0; "aaa", 1; "bbb", 1 ])
;;

let () = print_endline "All git parser tests passed."
