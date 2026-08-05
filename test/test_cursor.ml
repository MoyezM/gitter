open! Core
module D = Gitter.Leaves.Diff

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* A synthetic display: rule, 5 diff lines, rule, 5 diff lines. *)
let line n = D.Diff_line (Some n, Some n, Gitter.Git.Diff.Line.Context "x")
let rule = D.Hunk_header "@@ -1 +1 @@"

let lines =
  [ rule ] @ List.init 5 ~f:(fun i -> line i) @ [ rule ] @ List.init 5 ~f:(fun i -> line (i + 5))
;;

(* A long all-selectable display for scroll tests. *)
let many = List.init 100 ~f:line

(* snap: never rests on a rule. *)
let () =
  check "snap forward off rule" (D.snap lines ~dir:1 0 = 1);
  check "snap backward off rule" (D.snap lines ~dir:(-1) 6 = 5);
  check "snap stays on selectable" (D.snap lines ~dir:1 3 = 3);
  check "snap clamps at end" (D.snap lines ~dir:1 99 = 11);
  (* The fallback walks the OPPOSITE direction, even past several rules. *)
  let tail_rules = lines @ [ rule; rule ] in
  check "falls back past multiple rules" (D.snap tail_rules ~dir:1 13 = 11);
  let head_rules = [ rule; rule ] @ List.init 3 ~f:line in
  check "falls back forward past rules" (D.snap head_rules ~dir:(-1) 0 = 2);
  check "no diff lines: identity" (D.snap [ rule; rule ] ~dir:1 1 = 1)
;;

(* follow: keeps the cursor within scrolloff of the edges. *)
let () =
  let count = 100 in
  let height = 20 in
  check "no move when inside margins" (D.follow ~height ~count ~cursor:10 5 = 5);
  check "scrolls down to keep margin" (D.follow ~height ~count ~cursor:30 5 = 30 - (height - 1 - D.scrolloff));
  check "scrolls up to keep margin" (D.follow ~height ~count ~cursor:6 10 = 6 - D.scrolloff);
  check "clamps at top" (D.follow ~height ~count ~cursor:0 50 = 0);
  check "clamps at bottom" (D.follow ~height ~count ~cursor:99 0 = count - height)
;;

(* wheel: flat step per tick (velocity comes from the terminal's event
   rate), view moves and the cursor is clamped to stay visible. *)
let () =
  let state = D.initial_view_state in
  let s1 = D.wheel state many ~height:5 ~dir:1 in
  check "tick scrolls one base step" (s1.scroll = D.wheel_step);
  let s2 = D.wheel s1 many ~height:5 ~dir:1 in
  check "steps stay flat" (s2.scroll - s1.scroll = D.wheel_step);
  check "cursor clamped into view" (s2.cursor >= s2.scroll && s2.cursor < s2.scroll + 5);
  let s3 = D.wheel s2 many ~height:5 ~dir:(-1) in
  check "scrolls back up" (s3.scroll = s2.scroll - D.wheel_step);
  check "clamps at top" ((D.wheel state many ~height:5 ~dir:(-1)).scroll = 0)
;;

let () = print_endline "All cursor tests passed."
