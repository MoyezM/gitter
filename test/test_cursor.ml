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
let at cursor = { Model.cursor; scroll = 0; pan = 0 }

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
    ((apply many { Model.cursor = 23; scroll = 5; pan = 0 } (A.Move 1)).scroll = 24 - (20 - 1 - 5));
  check
    "scrolls up to keep margin"
    ((apply many { Model.cursor = 7; scroll = 3; pan = 0 } (A.Move (-1))).scroll = 6 - 5);
  check
    "no scroll while inside margins"
    ((apply many { Model.cursor = 10; scroll = 5; pan = 0 } (A.Move 1)).scroll = 5);
  (* Tiny panes shrink the margin instead of pinning the cursor off-screen. *)
  let m = apply ~height:5 many { Model.cursor = 49; scroll = 47; pan = 0 } (A.Move 1) in
  check "tiny pane keeps cursor visible" (m.scroll = 48 && m.cursor = 50);
  let m = apply ~height:1 many { Model.cursor = 7; scroll = 7; pan = 0 } (A.Move 1) in
  check "one-row pane pins scroll to cursor" (m.scroll = 8 && m.cursor = 8)
;;

(* Wheel: flat 3-row steps of the VIEW (velocity comes from the terminal's
   event rate); the cursor is dragged along only at the viewport edge. *)
let () =
  let m0 = { Model.cursor = 0; scroll = 0; pan = 0 } in
  let m1 = apply ~height:5 many m0 (A.Wheel 1) in
  check "wheel scrolls the view 3 rows" (m1.scroll = 3);
  check "wheel keeps cursor in view" (m1.cursor >= m1.scroll && m1.cursor < m1.scroll + 5);
  let m2 = apply ~height:5 many m1 (A.Wheel 1) in
  check "steps stay flat" (m2.scroll - m1.scroll = 3);
  let m3 = apply ~height:5 many m2 (A.Wheel (-1)) in
  check "wheel up steps back" (m3.scroll = m2.scroll - 3);
  check "wheel clamps at top" ((apply ~height:5 many m0 (A.Wheel (-1))).scroll = 0)
;;

(* Empty documents (binary-only diffs): every action is total — queued
   events can arrive against [||] and must not raise. *)
let () =
  let empty : Document.t = [||] in
  List.iter
    [ A.Move 1; A.Move (-1); A.Half_page 1; A.Wheel 1; A.Wheel (-1); A.Click 0; A.Reset ]
    ~f:(fun action ->
      let m = apply empty Model.initial action in
      check "empty doc action is total" (m.cursor = 0 && m.scroll = 0))
;;

(* An off-viewport cursor ghosts to the first visible diff row — what you
   see is what [j] moves from. *)
let () =
  check
    "off-screen cursor ghosts into the viewport"
    (State.effective_cursor many { Model.cursor = 90; scroll = 0; pan = 0 } ~height:5 = 0);
  let m = apply ~height:5 many { Model.cursor = 90; scroll = 0; pan = 0 } (A.Move 1) in
  check "move starts from the visible ghost" (m.cursor = 1)
;;

(* Reset, and the resize re-anchor: Click must hit the row render displayed
   after a pane growth left a stale scroll. *)
let () =
  let r = apply many { Model.cursor = 42; scroll = 40; pan = 0 } A.Reset in
  check "reset" (r.cursor = 0 && r.scroll = 0);
  let m = apply ~height:50 many { Model.cursor = 99; scroll = 95; pan = 0 } (A.Click 10) in
  check "click after resize hits the displayed row" (m.cursor = 60)
;;

(* Pan: 4-column steps, clamped to just past the longest visible line, and
   pinned at zero for short lines; motions preserve it, Reset clears it. *)
let () =
  let wide =
    Array.of_list
      [ rule; Document.Diff_line (Some 1, Some 1, Gitter.Git.Diff.Line.Context (String.make 100 'x')) ]
  in
  let m1 = apply wide Model.initial (A.Pan 1) in
  check "pan steps right" (m1.pan = 4);
  check "pan floor at zero" ((apply wide Model.initial (A.Pan (-1))).pan = 0);
  let far = Fn.apply_n_times ~n:50 (fun m -> apply wide m (A.Pan 1)) Model.initial in
  check "pan clamps past longest visible line" (far.pan = 100 - 8);
  check "short lines don't pan" ((apply many Model.initial (A.Pan 1)).pan = 0);
  check "motion preserves pan" ((apply wide m1 (A.Move 1)).pan = 4);
  check "reset clears pan" ((apply wide m1 A.Reset).pan = 0)
;;

(* hunk_at: which (file, hunk) encloses a row — drives hunk staging. *)
let () =
  let fh = Document.File_header "f" in
  let d = Array.of_list ([ rule; line 1; line 2; rule; line 3 ] : Document.line list) in
  check "row in first hunk" ([%equal: (int * int) option] (Document.hunk_at d ~row:1) (Some (0, 0)));
  check "header row belongs to its hunk" ([%equal: (int * int) option] (Document.hunk_at d ~row:3) (Some (0, 1)));
  check "row in second hunk" ([%equal: (int * int) option] (Document.hunk_at d ~row:4) (Some (0, 1)));
  let multi = Array.of_list [ fh; rule; line 1; fh; rule; line 2 ] in
  check "second file's hunk" ([%equal: (int * int) option] (Document.hunk_at multi ~row:5) (Some (1, 0)));
  check "row above any hunk" (Option.is_none (Document.hunk_at (Array.of_list [ fh ]) ~row:0));
  check "empty doc" (Option.is_none (Document.hunk_at [||] ~row:0))
;;

let () = print_endline "All cursor tests passed."
