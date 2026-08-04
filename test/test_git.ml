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
    ]
;;

let entries = Status.parse porcelain_sample

let find path =
  List.find_exn entries ~f:(fun (e : Status.Entry.t) -> String.equal e.path path)
;;

let () =
  check "entry count (headers skipped)" (List.length entries = 7);
  let unstaged = find "lib/app.ml" in
  check "unstaged: not staged" (not (Status.Entry.staged unstaged));
  check "unstaged: is unstaged" (Status.Entry.unstaged unstaged);
  let staged = find "bin/main.ml" in
  check "staged: is staged" (Status.Entry.staged staged);
  check "staged: not unstaged" (not (Status.Entry.unstaged staged));
  let both = find "lib/both.ml" in
  check "both: staged and unstaged" (Status.Entry.staged both && Status.Entry.unstaged both);
  let renamed = find "lib/new_name.ml" in
  check
    "rename carries origin"
    (Status.Entry.Kind.equal renamed.kind (Renamed { from = "lib/old_name.ml" }));
  check "rename is staged" (Status.Entry.staged renamed);
  let conflict = find "lib/conflict.ml" in
  check "unmerged kind" (Status.Entry.Kind.equal conflict.kind Unmerged);
  let untracked = find "test/scratch.ml" in
  check "untracked kind" (Status.Entry.Kind.equal untracked.kind Untracked);
  check "untracked not staged" (not (Status.Entry.staged untracked));
  check "path with space survives" (String.equal (find "docs/with space.md").path "docs/with space.md")
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
  let readme = List.last_exn files in
  check "second file path" (String.equal readme.path "README.md");
  check
    "second file lines"
    (List.equal
       Diff.Line.equal
       (List.hd_exn readme.hunks).lines
       [ Context "# gitter"; Added "A git TUI." ])
;;

let () = print_endline "All git parser tests passed."
