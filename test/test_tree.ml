open! Core
module Tree = Gitter.Panes.Files.Tree
module Sections = Gitter.Panes.Files.Sections
module State = Gitter.Panes.Files.State
module Entry = Gitter.Git.Status.Entry

let check name cond = if not cond then failwithf "FAILED: %s" name ()

let entry ?(kind = Entry.Kind.Changed) ?(index = 'M') ?(worktree = '.') path =
  { Entry.index; worktree; path; kind }
;;

(* git-sorted order, mirroring real porcelain output. *)
let entries =
  [ entry ".DS_Store"
  ; entry "REVIEW.md"
  ; entry "bench/bench_diff.ml"
  ; entry "bench/dune"
  ; entry "lib/git/queries.ml"
  ]
;;

let rows ?(collapsed = String.Set.empty) () = Tree.rows ~entries ~collapsed
let apply ?(model = State.Model.initial) action = State.apply_action ~entries model action

let name_of = function
  | Tree.Dir { name; _ } -> "d:" ^ name
  | File { name; _ } -> name
;;

(* Fully expanded: files in order, dirs opening their children, depths
   following nesting. *)
let () =
  let r = rows () in
  check
    "expanded shape"
    (List.equal
       String.equal
       (List.map r ~f:name_of)
       [ ".DS_Store"; "REVIEW.md"; "d:bench"; "bench_diff.ml"; "dune"; "d:lib"; "d:git"; "queries.ml" ]);
  check
    "depths follow nesting"
    (List.equal Int.equal (List.map r ~f:Tree.depth) [ 0; 0; 0; 1; 1; 0; 1; 2 ]);
  check
    "dir paths are full paths"
    (List.exists r ~f:(function
      | Tree.Dir { path = "lib/git"; depth = 1; _ } -> true
      | _ -> false))
;;

(* Collapsing hides children but keeps the dir row. *)
let () =
  let r = rows ~collapsed:(String.Set.of_list [ "bench" ]) () in
  check
    "collapsed dir keeps its row, hides children"
    (List.equal
       String.equal
       (List.map r ~f:name_of)
       [ ".DS_Store"; "REVIEW.md"; "d:bench"; "d:lib"; "d:git"; "queries.ml" ]);
  check
    "collapsed dir is marked"
    (List.exists r ~f:(function
      | Tree.Dir { path = "bench"; expanded = false; _ } -> true
      | _ -> false))
;;

(* Porcelain emits tracked entries before untracked ones, so one directory's
   files arrive as two separate runs — the tree must still show the
   directory exactly once. *)
let () =
  let sectioned =
    [ entry "lib/app.ml" (* tracked section *)
    ; entry ".DS_Store" ~kind:Entry.Kind.Untracked
    ; entry "lib/tree.ml" ~kind:Entry.Kind.Untracked (* untracked section *)
    ]
  in
  let r = Tree.rows ~entries:sectioned ~collapsed:String.Set.empty in
  check
    "sectioned input yields one dir"
    (List.count r ~f:(function
       | Tree.Dir { path = "lib"; _ } -> true
       | _ -> false)
     = 1);
  check
    "both files under the one dir"
    (List.equal
       String.equal
       (List.map r ~f:name_of)
       [ ".DS_Store"; "d:lib"; "app.ml"; "tree.ml" ])
;;

(* Actions: activate toggles dirs, collapse folds or jumps to parent,
   expand unfolds. Indices refer to the fully-expanded shape above. *)
let activate row = State.Action.Activate { row; height = 10 }
let collapse = State.Action.Collapse { height = 10 }

let sel key = { State.Model.initial with selection = Some key }
let selected (m : State.Model.t) = m.selection

let () =
  let m = apply (activate 2) in
  check "click a dir folds it and selects it"
    (Set.mem m.collapsed "bench" && [%equal: string option] (selected m) (Some "bench"));
  let m2 = State.apply_action ~entries m (activate 2) in
  check "click again unfolds" (not (Set.mem m2.collapsed "bench"));
  check "click a file just selects it"
    (let m = apply (activate 1) in
     Set.is_empty m.collapsed && [%equal: string option] (selected m) (Some "REVIEW.md"));
  let m = apply ~model:(sel "lib/git/queries.ml") collapse in
  check "collapse on a file jumps to its parent dir"
    ([%equal: string option] (selected m) (Some "lib/git") && Set.is_empty m.collapsed);
  let m = apply ~model:(sel "lib/git") collapse in
  check "collapse folds the dir under the selection" (Set.mem m.collapsed "lib/git");
  let m2 = State.apply_action ~entries m State.Action.Expand in
  check "expand unfolds it" (not (Set.mem m2.collapsed "lib/git"));
  check
    "top-level collapse is a no-op"
    ([%equal: string option] (selected (apply ~model:(sel ".DS_Store") collapse)) (Some ".DS_Store"))
;;

(* Selection: files select, dirs don't. *)
let () =
  let r = rows () in
  check
    "file row selects its full path"
    (Option.equal String.equal (State.selection r ~cursor:7) (Some "lib/git/queries.ml"));
  check "dir row selects nothing" (Option.is_none (State.selection r ~cursor:2))
;;

(* The staged/unstaged pane split: index side stages, worktree side (or
   being untracked/conflicted) keeps a file in Changes; MM sits in both. *)
let () =
  let staged e = Sections.is_staged e
  and unstaged e = Sections.is_unstaged e in
  check "M. is staged only" (staged (entry "a") && not (unstaged (entry "a")));
  check
    ".M is unstaged only"
    (let e = entry "b" ~index:'.' ~worktree:'M' in
     (not (staged e)) && unstaged e);
  check
    "MM is both"
    (let e = entry "c" ~worktree:'M' in
     staged e && unstaged e);
  check
    "untracked is unstaged"
    (let e = entry "z" ~kind:Entry.Kind.Untracked ~index:'?' ~worktree:'?' in
     (not (staged e)) && unstaged e);
  check
    "unmerged is unstaged"
    (let e = entry "w" ~kind:Entry.Kind.Unmerged ~index:'U' ~worktree:'U' in
     (not (staged e)) && unstaged e);
  check
    "staged rename"
    (let e = entry "r" ~kind:(Entry.Kind.Renamed { from = "old" }) ~index:'R' ~worktree:'.' in
     staged e && not (unstaged e))
;;

(* Wheel: scrolls the viewport without touching the selection; selection
   motion reveals it again with minimal scroll movement. *)
let () =
  let m = apply (State.Action.Wheel { dir = 1; height = 3 }) in
  check "wheel scrolls the viewport" (m.scroll = 3);
  check "wheel leaves the selection put" (Option.is_none (selected m));
  let m2 = State.apply_action ~entries m (State.Action.Wheel { dir = 1; height = 3 }) in
  check "wheel steps accumulate and clamp to the last page" (m2.scroll = 5);
  let m3 = State.apply_action ~entries m2 (State.Action.Move { dir = `Down; height = 3 }) in
  check "selection motion reveals it"
    ([%equal: string option] (selected m3) (Some "REVIEW.md") && m3.scroll = 1);
  let visible =
    State.apply_action
      ~entries
      { m with selection = Some "bench/dune" }
      (State.Action.Move { dir = `Down; height = 3 })
  in
  check "motion inside the viewport does not scroll"
    ([%equal: string option] (selected visible) (Some "lib") && visible.scroll = 3);
  let clicked = State.apply_action ~entries m (State.Action.Activate { row = 4; height = 3 }) in
  check "clicking a visible row keeps the viewport"
    ([%equal: string option] (selected clicked) (Some "bench/dune") && clicked.scroll = 3);
  check "offset clamps to the last page" (State.offset ~total:8 ~height:3 99 = 5);
  check "offset floors at zero" (State.offset ~total:8 ~height:3 (-2) = 0)
;;

(* The repair law: stable under reorder; a vanished key moves to its
   nearest surviving successor (then predecessor). *)
let () =
  let flat names = List.map names ~f:(fun n -> entry n) in
  let m =
    State.apply_action
      ~entries:(flat [ "a"; "b"; "c" ])
      (sel "b")
      State.Action.Rows_changed
  in
  check "survivor is kept" ([%equal: string option] (selected m) (Some "b"));
  check "keys are snapshotted" (List.equal String.equal m.keys [ "a"; "b"; "c" ]);
  let staged =
    State.apply_action ~entries:(flat [ "a"; "c" ]) m State.Action.Rows_changed
  in
  check "vanished key flows to its successor"
    ([%equal: string option] (selected staged) (Some "c"));
  let m2 =
    State.apply_action
      ~entries:(flat [ "a"; "c" ])
      staged
      State.Action.Rows_changed
  in
  let last_removed =
    State.apply_action ~entries:(flat [ "a" ]) m2 State.Action.Rows_changed
  in
  check "no successor falls back to the predecessor"
    ([%equal: string option] (selected last_removed) (Some "a"));
  check "pure repair is total on unknown keys"
    (Option.is_none
       (State.repair ~old_keys:[] ~selection:(Some "ghost") ~new_keys:[ "a" ]))
;;

(* Total on empty data. *)
let () =
  List.iter
    [ State.Action.Move { dir = `Up; height = 10 }
    ; Move { dir = `Down; height = 10 }
    ; Activate { row = 0; height = 10 }
    ; Collapse { height = 10 }
    ; Expand
    ; Wheel { dir = 1; height = 10 }
    ; Rows_changed
    ]
    ~f:(fun action ->
      let m = State.apply_action ~entries:[] State.Model.initial action in
      check "empty entries are total" (Option.is_none m.selection))
;;

let () = print_endline "All tree tests passed."
