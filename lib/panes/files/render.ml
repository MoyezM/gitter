open! Core
open! Bonsai_term

(* Pure View production for one file-tree pane. *)

let seg attrs s = View.text ~attrs s
let code_char c = if Char.equal c '.' then ' ' else c

(* The pane stays decoupled from Git_data (whose folder-level dependency
   would be circular): the root maps its load state to this. [`Empty]
   carries the pane's own idle message ("nothing staged" vs "working tree
   clean"). *)
type status =
  [ `Loading
  | `Error of Error.t
  | `Empty of string (* unconditional message — no tree regardless *)
  | `Loaded of string (* the idle message; tree-vs-message derived below (T5) *)
  ]

(* Each pane shows only ITS side's letter — the staged pane the index
   letter, Changes the worktree letter (real chars as parsed: an add/add
   conflict reads its true code, not a flattened one), Committed the
   name-status letter (carried in the index slot). *)
let status_code ~with_sel ~side (entry : Git.Status.Entry.t) =
  let c, attrs =
    match side, entry.kind with
    | (`Staged | `Unstaged), (Git.Status.Entry.Kind.Untracked | Unmerged) ->
      entry.worktree, Theme.untracked
    | `Staged, (Changed | Renamed _) -> entry.index, Theme.staged
    | `Unstaged, (Changed | Renamed _) -> entry.worktree, Theme.unstaged
    | `Committed, _ -> entry.index, [ Attr.fg Theme.blue ]
  in
  seg (with_sel attrs) (String.of_char (code_char c))
;;

(* Per-row +/- counts: files look up their path; directories sum their
   subtree (so a collapsed dir still shows what it holds). *)
let row_counts ~counts (r : Tree.row) =
  match r with
  | Tree.File { entry; _ } -> Map.find counts entry.Git.Status.Entry.path
  | Dir { path; _ } ->
    let prefix = path ^ "/" in
    let a, d =
      Map.fold counts ~init:(0, 0) ~f:(fun ~key ~data:(a2, d2) (a, d) ->
        if String.is_prefix key ~prefix then a + a2, d + d2 else a, d)
    in
    Option.some_if (a > 0 || d > 0) (a, d)
;;

let count_segs ~with_sel = function
  | None -> [], 0
  | Some (a, d) ->
    let sa = sprintf "+%d" a
    and sd = sprintf "-%d" d in
    ( [ seg (with_sel [ Attr.fg Theme.green ]) sa
      ; seg (with_sel []) " "
      ; seg (with_sel [ Attr.fg Theme.red ]) sd
      ; seg (with_sel []) " "
      ]
    , String.length sa + String.length sd + 2 )
;;

(* Row shape, lazygit-style: the whole row indents together, so the status
   code sits immediately beside its name instead of in a detached column;
   +/- counts sit right-aligned (dropped when the name leaves no room). *)
let row ~width ~counts ~reviewed ~selected ~side ~query (r : Tree.row) =
  let with_sel attrs = if selected then Theme.selection_bg :: attrs else attrs in
  let marker =
    if selected then seg (with_sel Theme.header) "\u{276F} " else seg (with_sel []) "  "
  in
  let indent depth = seg (with_sel []) (String.make (2 * depth) ' ') in
  let is_reviewed = Set.mem reviewed (State.row_key r) in
  let stats, stats_w =
    let stats, stats_w = count_segs ~with_sel (row_counts ~counts r) in
    (* Reviewed rows carry a check at the far right and recede visually
       — reviewed means done. *)
    if is_reviewed
    then stats @ [ seg (with_sel [ Attr.fg Theme.green ]) "\u{2713} " ], stats_w + 2
    else stats, stats_w
  in
  let dim attrs = if is_reviewed then Theme.context else attrs in
  let filled ~used segs =
    let fill = width - used - stats_w in
    if fill >= 1
    then View.hcat (segs @ [ seg (with_sel []) (String.make fill ' ') ] @ stats)
    else View.hcat (segs @ [ seg (with_sel []) (String.make (Int.max 0 (width - used)) ' ') ])
  in
  match r with
  | Tree.Dir { path; name; depth; expanded } ->
    let arrow = if expanded then "\u{25BE} " else "\u{25B8} " in
    let attrs = with_sel (dim Theme.header) in
    (* arrow+space is 2 cells (String.length would overcount the glyph). *)
    filled
      ~used:(2 + (2 * depth) + 2 + String.length name)
      ([ marker; indent depth; seg attrs arrow ] @ Search.Tree_search.underline ~query ~attrs ~candidate:path name)
  | File { entry; name; depth } ->
    let rename_suffix =
      match entry.kind with
      | Renamed { from } -> sprintf " \u{2190} %s" from
      | Changed | Untracked | Unmerged -> ""
    in
    let path = entry.Git.Status.Entry.path in
    let attrs = with_sel (dim [ Attr.fg Theme.text ]) in
    filled
      ~used:(2 + (2 * depth) + 2 + String.length name + String.length rename_suffix)
      ([ marker; indent depth; status_code ~with_sel ~side entry; seg (with_sel []) " " ]
       @ Search.Tree_search.underline ~query ~attrs ~candidate:path name
       @ (if String.is_empty rename_suffix then [] else [ seg attrs rename_suffix ]))
;;

let render
  ~(status : status)
  ~rows
  ~cursor
  ~scroll
  ~counts
  ~reviewed
  ~side
  ~search
  ~(dimensions : Dimensions.t)
  =
  match status with
  | `Loading -> seg Theme.context " loading git status..."
  | `Error e -> seg Theme.untracked (" git error: " ^ Error.to_string_hum e)
  | `Empty message -> seg Theme.context (" " ^ message)
  (* [rows] is the VISIBLE list — narrowed while a search is active — so
     emptiness only means "nothing to change" when no search is
     filtering: a zero-match query must render an empty tree with its
     0/N counter, not the idle message (T5). *)
  | `Loaded message
    when List.is_empty rows && not (Search.Prompt.is_active search) ->
    seg Theme.context (" " ^ message)
  | `Loaded (_ : string) ->
    List_view.render
      ~rows
      ~scroll
      ~cursor
      ~dimensions
      ~row:
        ((* Parse the query ONCE per render — [underline] runs per row. *)
         let query = Search.Prompt.parsed search in
         fun ~selected ~width r -> row ~width ~counts ~reviewed ~selected ~side ~query r)
;;
