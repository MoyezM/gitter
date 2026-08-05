open! Core
module Status = Gitter.Git.Status
module Diff = Gitter.Git.Diff

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

let () = print_endline "All git parser tests passed."
