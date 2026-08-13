open! Core
module Tree = Gitter.Panes.Files.Tree
module Entry = Gitter.Git.Status.Entry
module State = Gitter.Panes.Files.State
module Nav = Gitter.Tree_listing.Action

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
let apply ?(model = State.Model.initial) action =
  State.apply_action ~entries model (Gitter.Search.Action.Pane action)
;;

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
let activate row = State.Action.Nav (Nav.Activate { row; height = 10 })
let collapse = State.Action.Nav (Nav.Fold { height = 10 })

module Listing = Gitter.Listing

let sel key =
  { State.Model.initial with
    listing = { Listing.Model.initial with selection = Some key }
  }
;;

let selected (m : State.Model.t) = State.selection_key m

let () =
  let m = apply (activate 2) in
  check "click a dir folds it and selects it"
    (Set.mem m.fold "bench" && [%equal: string option] (selected m) (Some "bench"));
  let m2 = State.apply_action ~entries m (Gitter.Search.Action.Pane (activate 2)) in
  check "click again unfolds" (not (Set.mem m2.fold "bench"));
  check "click a file just selects it"
    (let m = apply (activate 1) in
     Set.is_empty m.fold && [%equal: string option] (selected m) (Some "REVIEW.md"));
  let m = apply ~model:(sel "lib/git/queries.ml") collapse in
  check "collapse on a file jumps to its parent dir"
    ([%equal: string option] (selected m) (Some "lib/git") && Set.is_empty m.fold);
  let m = apply ~model:(sel "lib/git") collapse in
  check "collapse folds the dir under the selection" (Set.mem m.fold "lib/git");
  let m2 = State.apply_action ~entries m (Gitter.Search.Action.Pane (State.Action.Nav (Nav.Unfold { height = 10 }))) in
  check "expand unfolds it" (not (Set.mem m2.fold "lib/git"));
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
  let staged e = Entry.is_staged e
  and unstaged e = Entry.is_unstaged e in
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
  let m = apply (State.Action.Nav (Nav.Wheel { dir = 1; height = 3 })) in
  check "wheel scrolls the viewport" (State.scroll m = 3);
  check "wheel leaves the selection put" (Option.is_none (selected m));
  let m2 = State.apply_action ~entries m (Gitter.Search.Action.Pane (State.Action.Nav (Nav.Wheel { dir = 1; height = 3 }))) in
  check "wheel steps accumulate and clamp to the last page" (State.scroll m2 = 5);
  let m3 = State.apply_action ~entries m2 (Gitter.Search.Action.Pane (State.Action.Nav (Nav.Move { dir = `Down; height = 3 }))) in
  check "selection motion reveals it"
    ([%equal: string option] (selected m3) (Some "REVIEW.md") && State.scroll m3 = 1);
  let visible =
    State.apply_action
      ~entries
      { m with listing = { m.listing with selection = Some "bench/dune" } }
      (Gitter.Search.Action.Pane (State.Action.Nav (Nav.Move { dir = `Down; height = 3 })))
  in
  check "motion inside the viewport does not scroll"
    ([%equal: string option] (selected visible) (Some "lib") && State.scroll visible = 3);
  let clicked = State.apply_action ~entries m (Gitter.Search.Action.Pane (State.Action.Nav (Nav.Activate { row = 4; height = 3 }))) in
  check "clicking a visible row keeps the viewport"
    ([%equal: string option] (selected clicked) (Some "bench/dune") && State.scroll clicked = 3);
  check "offset clamps to the last page" (Listing.offset ~total:8 ~height:3 99 = 5);
  check "offset floors at zero" (Listing.offset ~total:8 ~height:3 (-2) = 0)
;;

(* The repair law: stable under reorder; a vanished key moves to its
   nearest surviving successor (then predecessor). *)
let () =
  let flat names = List.map names ~f:(fun n -> entry n) in
  let m =
    State.apply_action
      ~entries:(flat [ "a"; "b"; "c" ])
      (sel "b")
      (Gitter.Search.Action.Pane (State.Action.Nav Nav.Rows_changed))
  in
  check "survivor is kept" ([%equal: string option] (selected m) (Some "b"));
  check "keys are snapshotted" (List.equal String.equal m.listing.keys [ "a"; "b"; "c" ]);
  let staged =
    State.apply_action ~entries:(flat [ "a"; "c" ]) m (Gitter.Search.Action.Pane (State.Action.Nav Nav.Rows_changed))
  in
  check "vanished key flows to its successor"
    ([%equal: string option] (selected staged) (Some "c"));
  let m2 =
    State.apply_action
      ~entries:(flat [ "a"; "c" ])
      staged
      (Gitter.Search.Action.Pane (State.Action.Nav Nav.Rows_changed))
  in
  let last_removed =
    State.apply_action ~entries:(flat [ "a" ]) m2 (Gitter.Search.Action.Pane (State.Action.Nav Nav.Rows_changed))
  in
  check "no successor falls back to the predecessor"
    ([%equal: string option] (selected last_removed) (Some "a"));
  check "pure repair is total on unknown keys"
    (Option.is_none
       (Listing.repair ~old_keys:[] ~selection:(Some "ghost") ~new_keys:[ "a" ]))
;;

(* Total on empty data. *)
let () =
  List.iter
    [ Gitter.Search.Action.Pane (State.Action.Nav (Nav.Move { dir = `Up; height = 10 }))
    ; Pane (Nav (Move { dir = `Down; height = 10 }))
    ; Pane (Nav (Activate { row = 0; height = 10 }))
    ; Pane (Nav (Fold { height = 10 }))
    ; Pane (Nav (Unfold { height = 10 }))
    ; Pane (Nav (Wheel { dir = 1; height = 10 }))
    ; Pane (Nav Rows_changed)
    ; Prompt { event = Gitter.Search.Prompt.Open; height = 10 }
    ; Prompt { event = Gitter.Search.Prompt.Commit; height = 10 }
    ; Prompt { event = Gitter.Search.Prompt.Cancel; height = 10 }
    ; Jump { dir = 1; height = 10 }
    ]
    ~f:(fun action ->
      let m = State.apply_action ~entries:[] State.Model.initial action in
      check "empty entries are total" (Option.is_none (State.selection_key m)))
;;

(* ---- search: narrow while typing, jump after --------------------------- *)

module Search = Gitter.Search

let s event ?(height = 10) model =
  State.apply_action ~entries model (Gitter.Search.Action.Prompt { event; height })
;;

let type_string model text =
  String.fold text ~init:model ~f:(fun m c -> s (Search.Prompt.Type (Char.to_string c)) m)
;;

(* Narrowing keeps the match's ancestor chain — a match is never
   orphaned — and matches count against the full tree. *)
let () =
  let m = type_string (s Search.Prompt.Open (sel "REVIEW.md")) "queries" in
  check
    "narrowed to the match plus its ancestors"
    (List.equal
       String.equal
       (List.map (State.visible_rows ~entries m) ~f:name_of)
       [ "d:lib"; "d:git"; "queries.ml" ]);
  check
    "counts are matches/total-rows"
    ([%equal: int * int]
       (State.match_counts ~entries m.search)
       (1, 8));
  check "the first match is selected live"
    ([%equal: string option] (selected m) (Some "lib/git/queries.ml"));
  check "the diff follows the live selection"
    ([%equal: string option]
       (let rows = State.visible_rows ~entries m in
        State.selection rows ~cursor:(State.index_of rows (State.selection_key m)))
       (Some "lib/git/queries.ml"))
;;

(* Collapsed directories are bypassed while filtering; the collapse set
   itself is untouched and Esc restores everything byte-for-byte. *)
let () =
  let folded =
    { (sel "REVIEW.md") with Gitter.Search.Tree_search.Model.fold = String.Set.of_list [ "lib" ] }
  in
  let m = type_string (s Search.Prompt.Open folded) "queries" in
  check
    "a match inside a folded dir surfaces"
    (List.mem (List.map (State.visible_rows ~entries m) ~f:name_of) "queries.ml" ~equal:String.equal);
  check "the collapse set is untouched while filtering"
    (Set.equal m.fold (String.Set.of_list [ "lib" ]));
  (* Wander with the arrows, then cancel: exact restore. *)
  let m = State.apply_action ~entries m (Gitter.Search.Action.Pane (State.Action.Nav (Nav.Move { dir = `Down; height = 10 }))) in
  let m = s Search.Prompt.Cancel m in
  check "esc restores the selection" ([%equal: string option] (selected m) (Some "REVIEW.md"));
  check "esc restores the collapse set" (Set.equal m.fold (String.Set.of_list [ "lib" ]));
  check "esc restores the scroll" (State.scroll m = State.scroll folded);
  check "esc leaves no register behind" (Option.is_none m.search.register)
;;

(* Zero matches: the tree renders empty, the pre-prompt selection stays
   effective (the diff keeps showing it), and nothing crashes through
   the repair path. *)
let () =
  let m = type_string (s Search.Prompt.Open (sel "bench/dune")) "zzz" in
  check "zero matches renders empty" (List.is_empty (State.visible_rows ~entries m));
  check
    "zero-match counts"
    ([%equal: int * int]
       (State.match_counts ~entries m.search)
       (0, 8));
  check "zero matches renders empty, selection restored to pre-prompt"
    ([%equal: string option] (State.selection_key m) (Some "bench/dune"));
  let repaired = State.apply_action ~entries m (Gitter.Search.Action.Pane (State.Action.Nav Nav.Rows_changed)) in
  check "filter churn at zero matches holds still"
    ([%equal: string option] (selected repaired) (Some "bench/dune"));
  (* Enter at zero matches: nothing was accepted; the register still
     commits. *)
  let committed = s Search.Prompt.Commit m in
  check "zero-match commit keeps the pre-prompt selection"
    ([%equal: string option] (selected committed) (Some "bench/dune"));
  check "zero-match commit still sets the register"
    ([%equal: string option] committed.search.register (Some "zzz"))
;;

(* Enter un-narrows, keeps the accepted selection, and expands whatever
   would hide it. *)
let () =
  let folded =
    { (sel "REVIEW.md") with Gitter.Search.Tree_search.Model.fold = String.Set.of_list [ "lib" ] }
  in
  let m = type_string (s Search.Prompt.Open folded) "queries" in
  let m = s Search.Prompt.Commit m in
  check "commit un-narrows"
    (List.length (State.visible_rows ~entries m) = 8);
  check "commit keeps the accepted selection"
    ([%equal: string option] (selected m) (Some "lib/git/queries.ml"));
  check "commit expanded the hiding ancestors" (not (Set.mem m.fold "lib"));
  check "commit sets the register" ([%equal: string option] m.search.register (Some "queries"));
  check "commit drops the snapshot" (Option.is_none m.snapshot)
;;

(* Committed n/N jump in the full tree, wrapping, expanding collapsed
   ancestors — ordinary expansions, kept. *)
let () =
  let committed =
    let m = type_string (s Search.Prompt.Open (sel ".DS_Store")) "dune" in
    s Search.Prompt.Commit m
  in
  (* Register "dune" matches bench/dune only; fold bench away, then jump
     back in. *)
  let folded = { committed with Gitter.Search.Tree_search.Model.fold = String.Set.of_list [ "bench" ] } in
  let m =
    State.apply_action ~entries folded (Gitter.Search.Action.Jump { dir = 1; height = 10 })
  in
  check "n reaches a match inside a folded dir"
    ([%equal: string option] (selected m) (Some "bench/dune"));
  check "the jump expanded the ancestor chain" (not (Set.mem m.fold "bench"));
  (* The single match wraps onto itself. *)
  let m2 = State.apply_action ~entries m (Gitter.Search.Action.Jump { dir = 1; height = 10 }) in
  check "n wraps" ([%equal: string option] (selected m2) (Some "bench/dune"));
  let m3 = State.apply_action ~entries m (Gitter.Search.Action.Jump { dir = -1; height = 10 }) in
  check "N wraps too" ([%equal: string option] (selected m3) (Some "bench/dune"));
  (* No register, no motion. *)
  let idle = State.apply_action ~entries (sel "REVIEW.md") (Gitter.Search.Action.Jump { dir = 1; height = 10 }) in
  check "n without a register is a no-op" ([%equal: string option] (selected idle) (Some "REVIEW.md"))
;;

(* Search.Action resolves mode-dependent keys against the CURRENT
   model at apply time — a same-frame "/d" burst must append, not
   discard (burst-collapse class). *)
let () =
  let both =
    Search.Action.By_mode
      { if_active = Some (Search.Action.Prompt { event = Search.Prompt.Type "d"; height = 10 })
      ; if_idle = Some (Search.Action.Pane (State.Action.Operate Discard))
      }
  in
  let resolve (model : State.Model.t) action =
    Search.Action.resolve ~search:model.search action
  in
  let active = s Search.Prompt.Open (sel "REVIEW.md") in
  check "active resolves to the typed reading"
    (match resolve active both with
     | Some (Search.Action.Prompt { event = Search.Prompt.Type "d"; _ }) -> true
     | _ -> false);
  let m = State.apply_action ~entries active both in
  check "the char lands in the query"
    ([%equal: string option] (Search.Prompt.query m.search) (Some "d"));
  check "idle resolves to the binding"
    (match resolve (sel "REVIEW.md") both with
     | Some (Search.Action.Pane (State.Action.Operate Discard)) -> true
     | _ -> false);
  check "a mode-dead key resolves to nothing"
    (Option.is_none
       (resolve
          (sel "REVIEW.md")
          (Search.Action.when_active (Search.Action.pane (State.Action.Nav (Nav.Unfold { height = 10 }))))))
;;

(* The jump gate: n with no register (or no match) is a true no-op —
   the host consults this before letting a jump claim the diff pane. *)
let () =
  check "no register, no jump" (not (State.can_jump ~entries (sel "REVIEW.md").Gitter.Search.Tree_search.Model.search));
  let committed =
    s Search.Prompt.Commit (type_string (s Search.Prompt.Open (sel ".DS_Store")) "dune")
  in
  check "a register with a live match jumps" (State.can_jump ~entries committed.search);
  let matchless =
    s Search.Prompt.Commit (type_string (s Search.Prompt.Open (sel ".DS_Store")) "zzz")
  in
  check "a register with no match does not" (not (State.can_jump ~entries matchless.search))
;;

(* A mid-prompt refresh that removes the matched file must not swap the
   diff to an unrelated one: the pre-prompt selection stays effective. *)
let () =
  let m = type_string (s Search.Prompt.Open (sel "bench/dune")) "queries" in
  check "the live match is selected"
    ([%equal: string option] (State.selection_key m) (Some "lib/git/queries.ml"));
  let shrunk = List.filter entries ~f:(fun e -> not (String.equal e.path "lib/git/queries.ml")) in
  let m = State.apply_action ~entries:shrunk m (Gitter.Search.Action.Pane (State.Action.Nav Nav.Rows_changed)) in
  check "after the refresh the pre-prompt snapshot still names bench/dune"
    ([%equal: string option]
       (match m.snapshot with
        | Some (listing, _) -> listing.Listing.Model.selection
        | None -> None)
       (Some "bench/dune"))
;;

(* The register survives content changes; the ghost re-commits on Enter
   over an empty prompt. *)
let () =
  let committed =
    let m = type_string (s Search.Prompt.Open (sel ".DS_Store")) "dune" in
    s Search.Prompt.Commit m
  in
  let reopened = s Search.Prompt.Open committed in
  check "reopened prompt shows all rows again"
    (List.length (State.visible_rows ~entries reopened) = 8);
  let recommitted = s Search.Prompt.Commit reopened in
  check "empty enter re-commits the ghost"
    ([%equal: string option] recommitted.search.register (Some "dune"));
  (* Esc from committed clears the register. *)
  let cleared = s Search.Prompt.Clear committed in
  check "esc clears the faded register" (Option.is_none cleared.search.register)
;;

let () = print_endline "All tree tests passed."
