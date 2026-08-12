open! Core
module Document = Gitter.Panes.Diff.Document
module State = Gitter.Panes.Diff.State
module Model = State.Model
module A = State.Action

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* A synthetic document: rule, 5 diff rows, rule, 5 diff rows. *)
(* [pos] is stamped by [Source.of_rows]. *)
let row line = { Document.line; revealed = false; pos = 0 }
let line n = row (Document.Diff_line (Some n, Some n, Gitter.Git.Diff.Line.Context "x"))
let rule () = row (Document.Rule { hidden = 0; old_no = 1; new_no = 1; label = "@@ -1 +1 @@" })

let rows = [ rule () ] @ List.init 5 ~f:line @ [ rule () ] @ List.init 5 ~f:(fun i -> line (i + 5))
let doc = Document.Source.of_rows rows

(* A long all-diff document for scroll tests. *)
let many = Document.Source.of_rows (List.init 100 ~f:line)
let apply ?(height = 20) d model action = State.apply_action d model action ~height
let at cursor = { Model.initial with Model.cursor }

(* The state machine transitions a SOURCE; these fixtures have no elided
   runs, so the document it builds is exactly the rows given. *)
let built src = State.shown src Model.initial

(* The effective cursor: fresh documents show the cursor on the first diff
   row, and motions start from what is shown. *)
let () =
  check
    "fresh cursor normalizes to first diff row"
    (State.effective_cursor (built doc) Model.initial = 1);
  check "j from fresh moves from the ghost" ((apply doc Model.initial (A.Move 1)).cursor = 2);
  check "k from fresh clamps to first diff row" ((apply doc Model.initial (A.Move (-1))).cursor = 1)
;;

(* Motions snap over rules; the fallback goes the other way, even far. *)
let () =
  check "move forward over a rule" ((apply doc (at 5) (A.Move 1)).cursor = 7);
  check "move backward over a rule" ((apply doc (at 7) (A.Move (-1))).cursor = 5);
  check "click lands on a diff row" ((apply doc (at 1) (A.Click { row = 0; column = 0 })).cursor = 1);
  check "click past the end clamps" ((apply doc (at 1) (A.Click { row = 99; column = 0 })).cursor = 11);
  let tail_rules = Document.Source.of_rows (rows @ [ rule (); rule () ]) in
  check
    "click on trailing rules falls back to the last diff row"
    ((apply tail_rules (at 1) (A.Click { row = 13; column = 0 })).cursor = 11);
  check "no diff rows: cursor stays put" ((apply (Document.Source.of_rows [ rule (); rule () ]) (at 1) (A.Move 1)).cursor = 1)
;;

(* Following: the cursor stays within the margin of the viewport edges. *)
let () =
  check
    "scrolls down to keep margin"
    ((apply many { Model.initial with Model.cursor = 23; scroll = 5 } (A.Move 1)).scroll = 24 - (20 - 1 - 5));
  check
    "scrolls up to keep margin"
    ((apply many { Model.initial with Model.cursor = 7; scroll = 3 } (A.Move (-1))).scroll = 6 - 5);
  check
    "no scroll while inside margins"
    ((apply many { Model.initial with Model.cursor = 10; scroll = 5 } (A.Move 1)).scroll = 5);
  (* Tiny panes shrink the margin instead of pinning the cursor off-screen. *)
  let m = apply ~height:5 many { Model.initial with Model.cursor = 49; scroll = 47 } (A.Move 1) in
  check "tiny pane keeps cursor visible" (m.scroll = 48 && m.cursor = 50);
  let m = apply ~height:1 many { Model.initial with Model.cursor = 7; scroll = 7 } (A.Move 1) in
  check "one-row pane pins scroll to cursor" (m.scroll = 8 && m.cursor = 8)
;;

(* Wheel: flat 3-row steps of the VIEW (velocity comes from the terminal's
   event rate); the cursor never moves — off-screen if need be. *)
let () =
  let m0 = { Model.initial with Model.cursor = 0; scroll = 0 } in
  let m1 = apply ~height:5 many m0 (A.Wheel 1) in
  check "wheel scrolls the view 3 rows" (m1.scroll = 3);
  check "wheel leaves the cursor put" (m1.cursor = 0);
  let m2 = apply ~height:5 many m1 (A.Wheel 1) in
  check "steps stay flat" (m2.scroll - m1.scroll = 3);
  check "cursor stays put once off-screen" (m2.cursor = 0);
  let m3 = apply ~height:5 many m2 (A.Wheel (-1)) in
  check "wheel up steps back" (m3.scroll = m2.scroll - 3);
  check "wheel clamps at top" ((apply ~height:5 many m0 (A.Wheel (-1))).scroll = 0)
;;

(* Empty documents (binary-only diffs): every action is total — queued
   events can arrive against [||] and must not raise. *)
let () =
  let empty = Document.Source.of_rows [] in
  List.iter
    [ A.Move 1
    ; A.Move (-1)
    ; A.Half_page 1
    ; A.Wheel 1
    ; A.Wheel (-1)
    ; A.Click { row = 0; column = 0 }
    ; A.Reveal
    ; A.Reset
    ]
    ~f:(fun action ->
      let m = apply empty Model.initial action in
      check "empty doc action is total" (m.cursor = 0 && m.scroll = 0))
;;

(* An off-viewport cursor stays authoritative: motions start from it and
   the view follows back to reveal it. *)
let () =
  check
    "off-screen cursor is not relocated"
    (let m = { Model.initial with Model.cursor = 90 } in
     State.effective_cursor (built many) m = 90);
  let m = apply ~height:5 many { Model.initial with Model.cursor = 90; scroll = 0 } (A.Move 1) in
  check "move starts from the real cursor" (m.cursor = 91);
  check "move reveals the cursor" (m.scroll <= m.cursor && m.cursor < m.scroll + 5)
;;

(* Reset; Click is an ABSOLUTE document row (the handler maps the clicked
   viewport row through the scroll it painted with). *)
let () =
  let r = apply many { Model.initial with Model.cursor = 42; scroll = 40 } A.Reset in
  check "reset" (r.cursor = 0 && r.scroll = 0);
  let m = apply ~height:50 many { Model.initial with Model.cursor = 99; scroll = 95 } (A.Click { row = 60; column = 0 }) in
  check "click is an absolute document row" (m.cursor = 60)
;;

(* Reveal: after a doc replacement the kept cursor re-snaps into the new
   document and the view moves only if needed to show it. *)
let () =
  let short = Document.Source.of_rows (List.init 12 ~f:line) in
  let m = apply ~height:5 short { Model.initial with Model.cursor = 90; scroll = 0 } A.Reveal in
  check "reveal snaps a beyond-doc cursor" (m.cursor = 11);
  check "reveal shows the snapped cursor" (m.scroll <= m.cursor && m.cursor < m.scroll + 5);
  let m = apply ~height:5 many { Model.initial with Model.cursor = 4; scroll = 2 } A.Reveal in
  check "reveal is a no-op for a visible cursor" (m.cursor = 4 && m.scroll = 2)
;;

(* Pan: 4-column steps, clamped to just past the longest visible line, and
   pinned at zero for short lines; motions preserve it, Reset clears it. *)
let () =
  let wide =
    Document.Source.of_rows
      [ rule (); row (Document.Diff_line (Some 1, Some 1, Gitter.Git.Diff.Line.Context (String.make 100 'x'))) ]
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

let () = print_endline "All cursor tests passed."
