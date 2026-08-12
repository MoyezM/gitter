open! Core
module Document = Gitter.Panes.Diff.Document
module State = Gitter.Panes.Diff.State
module Model = State.Model
module A = State.Action

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* A 60-line file with three one-line changes, as REAL git output at three
   context levels. The whole design rests on [of_source ~context:c] being
   the body of [git diff -U(diff.context + c)], so it is pinned against
   git itself rather than against a hand-derived expectation. *)
let u3 =
  String.concat_lines
    [ "diff --git a/f.txt b/f.txt"
    ; "index 8b2034d..e81b4f6 100644"
    ; "--- a/f.txt"
    ; "+++ b/f.txt"
    ; "@@ -7,7 +7,7 @@ line 6"
    ; " line 7"
    ; " line 8"
    ; " line 9"
    ; "-line 10"
    ; "+CHANGED 10"
    ; " line 11"
    ; " line 12"
    ; " line 13"
    ; "@@ -27,7 +27,7 @@ line 26"
    ; " line 27"
    ; " line 28"
    ; " line 29"
    ; "-line 30"
    ; "+CHANGED 30"
    ; " line 31"
    ; " line 32"
    ; " line 33"
    ; "@@ -47,7 +47,7 @@ line 46"
    ; " line 47"
    ; " line 48"
    ; " line 49"
    ; "-line 50"
    ; "+CHANGED 50"
    ; " line 51"
    ; " line 52"
    ; " line 53"
    ]
;;

let u5 =
  String.concat_lines
    [ "diff --git a/f.txt b/f.txt"
    ; "index 8b2034d..e81b4f6 100644"
    ; "--- a/f.txt"
    ; "+++ b/f.txt"
    ; "@@ -5,11 +5,11 @@ line 4"
    ; " line 5"
    ; " line 6"
    ; " line 7"
    ; " line 8"
    ; " line 9"
    ; "-line 10"
    ; "+CHANGED 10"
    ; " line 11"
    ; " line 12"
    ; " line 13"
    ; " line 14"
    ; " line 15"
    ; "@@ -25,11 +25,11 @@ line 24"
    ; " line 25"
    ; " line 26"
    ; " line 27"
    ; " line 28"
    ; " line 29"
    ; "-line 30"
    ; "+CHANGED 30"
    ; " line 31"
    ; " line 32"
    ; " line 33"
    ; " line 34"
    ; " line 35"
    ; "@@ -45,11 +45,11 @@ line 44"
    ; " line 45"
    ; " line 46"
    ; " line 47"
    ; " line 48"
    ; " line 49"
    ; "-line 50"
    ; "+CHANGED 50"
    ; " line 51"
    ; " line 52"
    ; " line 53"
    ; " line 54"
    ; " line 55"
    ]
;;

let u8 =
  String.concat_lines
    [ "diff --git a/f.txt b/f.txt"
    ; "index 8b2034d..e81b4f6 100644"
    ; "--- a/f.txt"
    ; "+++ b/f.txt"
    ; "@@ -2,17 +2,17 @@ line 1"
    ; " line 2"
    ; " line 3"
    ; " line 4"
    ; " line 5"
    ; " line 6"
    ; " line 7"
    ; " line 8"
    ; " line 9"
    ; "-line 10"
    ; "+CHANGED 10"
    ; " line 11"
    ; " line 12"
    ; " line 13"
    ; " line 14"
    ; " line 15"
    ; " line 16"
    ; " line 17"
    ; " line 18"
    ; "@@ -22,17 +22,17 @@ line 21"
    ; " line 22"
    ; " line 23"
    ; " line 24"
    ; " line 25"
    ; " line 26"
    ; " line 27"
    ; " line 28"
    ; " line 29"
    ; "-line 30"
    ; "+CHANGED 30"
    ; " line 31"
    ; " line 32"
    ; " line 33"
    ; " line 34"
    ; " line 35"
    ; " line 36"
    ; " line 37"
    ; " line 38"
    ; "@@ -42,17 +42,17 @@ line 41"
    ; " line 42"
    ; " line 43"
    ; " line 44"
    ; " line 45"
    ; " line 46"
    ; " line 47"
    ; " line 48"
    ; " line 49"
    ; "-line 50"
    ; "+CHANGED 50"
    ; " line 51"
    ; " line 52"
    ; " line 53"
    ; " line 54"
    ; " line 55"
    ; " line 56"
    ; " line 57"
    ; " line 58"
    ]
;;

let old_text = String.concat_lines (List.init 60 ~f:(fun i -> sprintf "line %d" (i + 1)))

let new_text =
  String.concat_lines
    (List.init 60 ~f:(fun i ->
       match i + 1 with
       | 10 | 30 | 50 -> sprintf "CHANGED %d" (i + 1)
       | n -> sprintf "line %d" n))
;;

let source diff = Document.Source.create (Gitter.Git.Diff.parse diff) ~old_text ~new_text
let src = source u3
(* Uniform symmetric levels across every run — the git -UN equivalence
   case. *)
let uniform src c =
  List.fold (Document.runs src) ~init:Int.Map.empty ~f:(fun m key -> Map.set m ~key ~data:(c, c))
;;

let at c = Document.of_source src ~levels:(uniform src c)

(* The deepest any end of any run goes — the "whole file" level. *)
let full_level =
  List.fold (Document.runs src) ~init:0 ~f:(fun a k ->
    let t, b = Document.run_max src k in
    Int.max a (Int.max t b))
;;

(* First display row satisfying [f] — the scaffold every lookup shares. *)
let find_row doc ~f =
  Array.findi doc ~f:(fun _ (r : Document.row) -> f r.Document.line)
  |> Option.value_exn
  |> fst
;;

let changed_row doc text =
  find_row doc ~f:(function
    | Document.Diff_line (_, _, Added a) -> String.equal a text
    | _ -> false)
;;

(* Revealed (top, bottom) of the run keyed [key]; absent = (0, 0). *)
let level_at (m : Model.t) key = Option.value (Map.find m.Model.levels key) ~default:(0, 0)

let marker_row ?(nth = 0) doc =
  let seen = ref (-1) in
  find_row doc ~f:(function
    | Document.Rule { hidden; _ } when hidden > 0 ->
      incr seen;
      !seen = nth
    | _ -> false)
;;
let apply ?(height = 20) s model action = State.apply_action s model action ~height

(* The content rows and their numbers — what "the same diff body" means. *)
let body doc =
  Array.to_list doc
  |> List.filter_map ~f:(fun (r : Document.row) ->
    match r.line with
    | Document.Diff_line (o, n, l) ->
      let tag, s =
        match l with
        | Context s -> " ", s
        | Added s -> "+", s
        | Removed s -> "-", s
      in
      Some
        (sprintf
           "%s %s|%s %s"
           tag
           (Option.value_map o ~default:"-" ~f:Int.to_string)
           (Option.value_map n ~default:"-" ~f:Int.to_string)
           s)
    | _ -> None)
;;

let hidden doc =
  Array.to_list doc
  |> List.filter_map ~f:(fun (r : Document.row) ->
    match r.line with
    | Document.Rule { hidden; _ } when hidden > 0 -> Some hidden
    | _ -> None)
;;

(* Rules of any kind: counted or plain — the boundaries left on screen. *)
let rules doc =
  Array.count doc ~f:(fun (r : Document.row) ->
    match r.line with
    | Document.Rule _ -> true
    | _ -> false)
;;

(* THE property. Raising the level by n must produce exactly the diff git
   produces at -U(3 + n) — body, line numbers and elided counts alike. *)
let () =
  List.iter
    [ 0, u3, "u3"; 5, u5, "u5"; 8, u8, "u8" ]
    ~f:(fun (c, reference, name) ->
      let mine = at c
      and theirs = Document.of_source (source reference) ~levels:Int.Map.empty in
      check
        (sprintf "context %d has the body of git %s" c name)
        ([%equal: string list] (body mine) (body theirs));
      check
        (sprintf "context %d elides what git %s elides" c name)
        ([%equal: int list] (hidden mine) (hidden theirs)))
;;

(* Level 0 is what git shipped, so a reader who never presses a key — and
   whoever set diff.context — sees exactly today's diff. The base level is
   RECOVERED from the data; we are never told diff.context. *)
let () =
  check "level 0 preserves what git shipped" ([%equal: int list] (hidden (at 0)) [ 6; 13; 13; 7 ]);
  check "the base context is recovered from the data" (Document.base_context src = 3);
  check
    "so the first rung above it always changes something"
    (Array.length (at 7) > Array.length (at 0))
;;

(* Locality: raising one run's level moves NOTHING anywhere else — the
   other runs' counts are untouched, which is also what keeps expansion
   from feeling like the whole file breathing. *)
let () =
  let keys = Document.runs src in
  check "the fixture has several runs" (List.length keys = 4);
  let key = List.nth_exn keys 1 in
  let doc = Document.of_source src ~levels:(Int.Map.singleton key (60, 60)) in
  check
    "one run fully open, every other count exactly as at rest"
    ([%equal: int list] (hidden doc) [ 6; 13; 7 ]);
  check "and only that run's rows appeared" (Array.length doc = Array.length (at 0) - 1 + 13)
;;

(* Enough context and the hunks merge: no marker, and no rule between
   them either, which is what "do what github does" means. *)
let () =
  let full = at full_level in
  check "the top rung hides nothing" (List.is_empty (hidden full));
  check "and leaves a single continuous region" (rules full <= 1);
  check "showing the whole file" (List.length (body full) = 63)
;;

(* Staging granularity is NOT a function of the context level: the hunk a
   row stages is resolved from its LINE NUMBER against the original parse,
   which the mask cannot touch. git itself would have merged these hunks
   at high context. *)
let () =
  let targets c =
    let doc = at c in
    Array.filter_mapi doc ~f:(fun r (row : Document.row) ->
      match row.line with
      | Document.Diff_line (_, _, (Added _ | Removed _)) ->
        Document.hunk_under src doc ~row:r
        |> Option.map ~f:(fun (h : Gitter.Git.Diff.Hunk.t) -> h.old_start)
      | _ -> None)
    |> Array.to_list
  in
  let want = [ 7; 7; 27; 27; 47; 47 ] in
  List.iter [ 0; 2; 5; 40 ] ~f:(fun c ->
    check
      (sprintf "at context %d, s still stages the hunk under the cursor" c)
      ([%equal: int list] (targets c) want))
;;

(* Ownership is by line number: gap rows go to the NEARER hunk, a counted
   rule to the hunk it labels (below), and out of range to nothing. *)
let () =
  let doc = at full_level in
  let owner_of text =
    let r =
      find_row doc ~f:(function
        | Document.Diff_line (_, _, Context s) -> String.equal s text
        | _ -> false)
    in
    Document.hunk_under src doc ~row:r |> Option.map ~f:(fun h -> h.Gitter.Git.Diff.Hunk.old_start)
  in
  check "a revealed row near the first change stages it" ([%equal: int option] (owner_of "line 4") (Some 7));
  check "midway rows go to the nearer hunk" ([%equal: int option] (owner_of "line 15") (Some 7));
  check "and past the midpoint, to the next" ([%equal: int option] (owner_of "line 22") (Some 27));
  let rest = at 0 in
  let rule_row = marker_row rest in
  check
    "a counted rule stages the hunk below it"
    ([%equal: int option]
       (Document.hunk_under src rest ~row:rule_row
        |> Option.map ~f:(fun h -> h.Gitter.Git.Diff.Hunk.old_start))
       (Some 7));
  check "out of range stages nothing" (Option.is_none (Document.hunk_under src rest ~row:999));
  (* Multi-file sources never stage — line numbers repeat across files —
     and an empty document has nothing under any row. *)
  let multi =
    Document.Source.create
      (Gitter.Git.Diff.parse
         (String.concat_lines
            ([ "diff --git a/a.txt b/a.txt"; "@@ -1,2 +1,2 @@"; " x"; "-y"; "+Y" ]
             @ [ "diff --git a/b.txt b/b.txt"; "@@ -1,2 +1,2 @@"; " x"; "-y"; "+Y" ])))
      ~old_text:""
      ~new_text:""
  in
  let mdoc = Document.of_source multi ~levels:Int.Map.empty in
  check
    "multi-file sources stage nothing anywhere"
    (Array.for_alli mdoc ~f:(fun r _ -> Option.is_none (Document.hunk_under multi mdoc ~row:r)));
  check
    "and their file headers are unowned in particular"
    (match mdoc.(0).Document.line with
     | Document.File_header _ -> Option.is_none (Document.hunk_under multi mdoc ~row:0)
     | _ -> false);
  let empty = Document.Source.empty in
  check
    "an empty document stages nothing"
    (Option.is_none
       (Document.hunk_under empty (Document.of_source empty ~levels:Int.Map.empty) ~row:0))
;;

(* The ladder, and the headline invariant: the cursor is a file position,
   so a mask change does not touch it AT ALL — the same position simply
   displays on whatever the new mask shows. *)
let () =
  check "levels start at what git shipped" (Map.is_empty Model.initial.Model.levels);
  (* The cursor starts on the leading marker, so K acts on ITS run — and
     on its BOTTOM end only, the lines directly above the first hunk. The
     leading run is shallower than the first rung, so one press opens it
     fully rather than teasing four lines out of it. *)
  let key = List.hd_exn (Document.runs src) in
  let level m = level_at m key in
  let m1 = apply src Model.initial (A.Context `Up) in
  check
    "one press fully opens a run shallower than the first rung"
    (snd (level m1) = snd (Document.run_max src key));
  check "and leaves the top end alone" (fst (level m1) = 0);
  check "and only that run" (Map.length m1.Model.levels = 1);
  check
    "a further press saturates"
    ([%equal: int * int] (level (apply src m1 (A.Context `Up))) (level m1));
  check
    "Reset returns that run to what git shipped"
    (Map.is_empty (apply src m1 (A.Context `Reset)).Model.levels);
  (* A deeper run climbs the ladder rung by rung. *)
  let mid_key = List.nth_exn (Document.runs src) 1 in
  let doc = at 0 in
  let inside = changed_row doc "CHANGED 30" in
  let mid m = level_at m mid_key in
  let n1 = apply src { Model.initial with Model.cursor = doc.(inside).Document.pos } (A.Context `Up) in
  let n2 = apply src n1 (A.Context `Up) in
  check "a deep run keeps climbing" (snd (mid n2) > snd (mid n1) && snd (mid n1) > 0);
  (* Park the cursor on a changed line; walk the whole ladder and back. *)
  let doc = at 0 in
  let row = changed_row doc "CHANGED 30" in
  let m0 = { Model.initial with Model.cursor = doc.(row).Document.pos } in
  let m = apply src (apply src (apply src m0 (A.Context `Up)) (A.Context `Down)) (A.Context `Up) in
  check "no mask change ever moved the cursor" (m.Model.cursor = m0.Model.cursor);
  let doc = State.shown src m in
  check
    "and it still displays on the line it was on"
    (match doc.(State.effective_cursor doc m).Document.line with
     | Document.Diff_line (_, _, Added a) -> String.equal a "CHANGED 30"
     | _ -> false);
  (* Fold everything away: the position now displays on the marker hiding
     it — [X] never strands the cursor. *)
  let hidden_cursor = { m0 with Model.cursor = (at 8).(1).Document.pos } in
  let folded = apply src hidden_cursor (A.Context `Reset) in
  let doc = State.shown src folded in
  check
    "a position the mask hides displays on its marker"
    (match doc.(State.effective_cursor doc folded).Document.line with
     | Document.Rule { hidden; _ } -> hidden > 0
     | _ -> false)
;;

(* Repeated asks return the same array: render, the handler and every
   action all call this, and a 646K-row document must not be rebuilt
   several times a frame. *)
let () =
  check "the built document is memoized" (phys_equal (at 3) (at 3));
  check "and a different level rebuilds" (not (phys_equal (at 3) (at 4)))
;;

(* Fail closed, silently: without a trustworthy pre-image there is nothing
   to reveal, and the pane renders exactly what it renders today. *)
let () =
  let no_reveal src =
    let a = Document.of_source src ~levels:Int.Map.empty
    and b = Document.of_source src ~levels:(uniform src 50) in
    [%equal: string list] (body a) (body b) && List.is_empty (hidden a)
  in
  check
    "a blob that is a different file offers nothing"
    (no_reveal
       (Document.Source.create
          (Gitter.Git.Diff.parse u3)
          ~old_text:(String.concat_lines (List.init 60 ~f:(sprintf "OTHER %d")))
          ~new_text));
  check
    "an absent pre-image offers nothing"
    (no_reveal (Document.Source.create (Gitter.Git.Diff.parse u3) ~old_text:"" ~new_text))
;;

(* The viewport must not jump on a mask change, and a DIRECTIONAL reveal
   grows the view away from its attachment side: expanding up pins the
   content below the boundary — the new rows push the view upward — and
   expanding down pins the content above. Fold keeps the cursor's exact
   screen line; with even that off screen (parked by a wheel scroll), the
   top row holds and the viewport does NOT chase the cursor: [Context] is
   not a motion. *)
let () =
  let height = 20 in
  let apply m a = State.apply_action src m a ~height in
  (* Cursor on the third marker, low enough on screen that the boundary
     can recede a full rung without leaving the viewport, and with the
     document long enough that no clamp interferes with the pin. *)
  let doc = at 0 in
  let marker = marker_row ~nth:2 doc in
  let m = { Model.initial with Model.cursor = doc.(marker).Document.pos; scroll = marker - 11 } in
  let line_of_pos (m : Model.t) pos =
    Document.index_of_pos (State.shown src m) pos - m.Model.scroll
  in
  (* K: the first row BELOW the boundary holds its screen line, and the
     boundary itself recedes upward. *)
  let below = doc.(marker + 1).Document.pos in
  let m' = apply m (A.Context `Up) in
  check "K pins the content below the boundary" (line_of_pos m' below = line_of_pos m below);
  check
    "so the boundary recedes upward"
    (line_of_pos m' m.Model.cursor < line_of_pos m m.Model.cursor);
  check "and the cursor's position never moves" (m'.Model.cursor = m.Model.cursor);
  (* J: the last row ABOVE the boundary holds instead. *)
  let above = doc.(marker - 1).Document.pos in
  let j = apply m (A.Context `Down) in
  check "J pins the content above the boundary" (line_of_pos j above = line_of_pos m above);
  (* X: fold-style — the cursor's row keeps its screen line. Staged from
     an [`Open] high in the document, where the shrink cannot run into the
     scroll clamp. *)
  let line_of (m : Model.t) = State.effective_cursor (State.shown src m) m - m.Model.scroll in
  let early = marker_row ~nth:1 doc in
  let o =
    apply
      { Model.initial with Model.cursor = doc.(early).Document.pos; scroll = early - 5 }
      (A.Context `Open)
  in
  let x = apply o (A.Context `Reset) in
  check "X keeps the cursor's screen line" (line_of x = line_of o);
  (* Cursor parked at the top, viewport wheeled to the bottom. *)
  let bottom = { Model.initial with Model.scroll = Array.length doc - height } in
  let top_content (m : Model.t) =
    (State.shown src m).(State.clamp_scroll (State.shown src m) ~height m.Model.scroll).Document.pos
  in
  let b' = apply bottom (A.Context `Up) in
  check
    "K with everything off screen holds the top row instead of chasing"
    (top_content b' = top_content bottom);
  check "and still does not move the cursor" (b'.Model.cursor = 0)
;;

(* A clicked rule becomes the cursor, so the run stays at the pointer. *)
let () =
  let doc = at 0 in
  let marker = marker_row doc in
  let m = apply src Model.initial (A.Click { row = marker; column = 40 }) in
  check "clicking a rule adopts it as the cursor" (m.Model.cursor = doc.(marker).Document.pos);
  check "and raises exactly one run's level" (Map.length m.Model.levels = 1);
  (* The chevrons choose the direction — and their zones come from the
     same definition as the glyphs, so they cannot drift apart again. *)
  let key = List.hd_exn (Document.runs src) in
  let level m = level_at m key in
  let click c = apply src Model.initial (A.Click { row = marker; column = c }) in
  let t_up, b_up = level (click 12) in
  check "clicking \u{25B2} opens the bottom end only" (t_up = 0 && b_up > 0);
  let t_dn, b_dn = level (click 18) in
  check "clicking \u{25BC} opens the top end only" (t_dn > 0 && b_dn = 0);
  let t_o, b_o = level (click 40) in
  check "clicking the rule elsewhere opens both" (t_o > 0 && b_o > 0)
;;

(* Directionality: K opens a boundary's BOTTOM — the lines that appear
   directly above the reading position — so the run's first hidden line
   (the marker's own number) does not move; J opens its TOP, which does
   move it. And each acts on the boundary on ITS side of the cursor. *)
let () =
  let marker_at doc key =
    Array.find_map doc ~f:(fun (r : Document.row) ->
      match r.line with
      | Document.Rule { hidden; old_no; _ } when hidden > 0 && old_no >= key -> Some (old_no, hidden)
      | _ -> None)
  in
  (* Cursor inside the SECOND hunk: K must reach the boundary above it
     (the middle run), J the one below (also visible from here). *)
  let doc = at 0 in
  let inside = changed_row doc "CHANGED 30" in
  let m0 = { Model.initial with Model.cursor = doc.(inside).Document.pos; scroll = 0 } in
  let middle_key = List.nth_exn (Document.runs src) 1 in
  let up = apply src m0 (A.Context `Up) in
  let o0, h0 = Option.value_exn (marker_at (at 0) middle_key) in
  let o1, h1 = Option.value_exn (marker_at (State.shown src up) middle_key) in
  check "K shrinks the boundary above the cursor" (h1 < h0);
  check "from its bottom: the first hidden line does not move" (o1 = o0);
  check "and touches exactly one run" (Map.length up.Model.levels = 1);
  check
    "and only its bottom end"
    (match Map.find up.Model.levels middle_key with
     | Some (t, b) -> t = 0 && b > 0
     | None -> false);
  let down = apply src m0 (A.Context `Down) in
  let third_key = List.nth_exn (Document.runs src) 2 in
  check "J acts on the boundary BELOW the cursor" (Map.mem down.Model.levels third_key);
  let o2, h2 = Option.value_exn (marker_at (State.shown src down) third_key) in
  let o0', h0' = Option.value_exn (marker_at (at 0) third_key) in
  check "and opens its top: the first hidden line moves down" (o2 > o0' && h2 < h0')
;;

(* Totality: every action against a document with no rows. *)
let () =
  List.iter
    [ A.Move 1
    ; A.Half_page 1
    ; A.Wheel 1
    ; A.Click { row = 0; column = 0 }
    ; A.Click { row = 5; column = 0 }
    ; A.Context `Up
    ; A.Context `Down
    ; A.Context `Open
    ; A.Context `Reset
    ; A.Reveal
    ; A.Reset
    ]
    ~f:(fun action ->
      List.iter [ Document.Source.of_rows []; Document.Source.empty ] ~f:(fun s ->
        let m = apply s Model.initial action in
        check "action on an empty document is total" (m.cursor = 0 && m.scroll = 0)))
;;

let () = print_endline "All expand tests passed."
