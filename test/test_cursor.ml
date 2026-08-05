open! Core
module Document = Gitter.Panes.Diff.Document
module State = Gitter.Panes.Diff.State
module Model = State.Model
module A = State.Action

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* A synthetic document: rule, 5 diff rows, rule, 5 diff rows. *)
let line n = Document.Diff_line (Some n, Some n, Gitter.Git.Diff.Line.Context "x")
let rule = Document.Hunk_header "@@ -1 +1 @@"

let doc =
  Array.of_list
    ([ rule ] @ List.init 5 ~f:line @ [ rule ] @ List.init 5 ~f:(fun i -> line (i + 5)))
;;

(* A long all-diff document for scroll tests. *)
let many = Array.init 100 ~f:line
let apply ?(height = 20) d model action = State.apply_action d model action ~height
let at cursor = { Model.cursor; scroll = 0 }

(* The effective cursor: fresh documents show the cursor on the first diff
   row, and motions start from what is shown. *)
let () =
  check
    "fresh cursor ghosts to first diff row"
    (State.effective_cursor doc Model.initial ~height:20 = 1);
  check "j from fresh moves from the ghost" ((apply doc Model.initial (A.Move 1)).cursor = 2);
  check "k from fresh clamps to first diff row" ((apply doc Model.initial (A.Move (-1))).cursor = 1)
;;

(* Motions snap over rules; the fallback goes the other way, even far. *)
let () =
  check "move forward over a rule" ((apply doc (at 5) (A.Move 1)).cursor = 7);
  check "move backward over a rule" ((apply doc (at 7) (A.Move (-1))).cursor = 5);
  check "click lands on a diff row" ((apply doc (at 1) (A.Click 0)).cursor = 1);
  check "click past the end clamps" ((apply doc (at 1) (A.Click 99)).cursor = 11);
  let tail_rules = Array.append doc [| rule; rule |] in
  check
    "click on trailing rules falls back to the last diff row"
    ((apply tail_rules (at 1) (A.Click 13)).cursor = 11);
  check "no diff rows: cursor stays put" ((apply [| rule; rule |] (at 1) (A.Move 1)).cursor = 1)
;;

(* Following: the cursor stays within the margin of the viewport edges. *)
let () =
  check
    "scrolls down to keep margin"
    ((apply many { Model.cursor = 29; scroll = 5 } (A.Move 1)).scroll = 30 - (20 - 1 - 5));
  check
    "scrolls up to keep margin"
    ((apply many { Model.cursor = 7; scroll = 10 } (A.Move (-1))).scroll = 6 - 5);
  check
    "no scroll while inside margins"
    ((apply many { Model.cursor = 10; scroll = 5 } (A.Move 1)).scroll = 5);
  (* Tiny panes shrink the margin instead of pinning the cursor off-screen. *)
  let m = apply ~height:5 many { Model.cursor = 49; scroll = 47 } (A.Move 1) in
  check "tiny pane keeps cursor visible" (m.scroll = 48 && m.cursor = 50);
  let m = apply ~height:1 many { Model.cursor = 7; scroll = 7 } (A.Move 1) in
  check "one-row pane pins scroll to cursor" (m.scroll = 8 && m.cursor = 8)
;;

(* Wheel: flat 3-row steps of the VIEW (velocity comes from the terminal's
   event rate); the cursor is dragged along only at the viewport edge. *)
let () =
  let m0 = { Model.cursor = 0; scroll = 0 } in
  let m1 = apply ~height:5 many m0 (A.Wheel 1) in
  check "wheel scrolls the view 3 rows" (m1.scroll = 3);
  check "wheel keeps cursor in view" (m1.cursor >= m1.scroll && m1.cursor < m1.scroll + 5);
  let m2 = apply ~height:5 many m1 (A.Wheel 1) in
  check "steps stay flat" (m2.scroll - m1.scroll = 3);
  let m3 = apply ~height:5 many m2 (A.Wheel (-1)) in
  check "wheel up steps back" (m3.scroll = m2.scroll - 3);
  check "wheel clamps at top" ((apply ~height:5 many m0 (A.Wheel (-1))).scroll = 0)
;;

(* Reset, and the resize re-anchor: Click must hit the row render displayed
   after a pane growth left a stale scroll. *)
let () =
  let r = apply many { Model.cursor = 42; scroll = 40 } A.Reset in
  check "reset" (r.cursor = 0 && r.scroll = 0);
  let m = apply ~height:50 many { Model.cursor = 99; scroll = 95 } (A.Click 10) in
  check "click after resize hits the displayed row" (m.cursor = 60)
;;

let () = print_endline "All cursor tests passed."
