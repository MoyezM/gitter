open! Core
open! Bonsai_term

(* Pure View production for one file-tree pane. *)

let seg attrs s = View.text ~attrs s
let pad_to width s = s ^ String.make (Int.max 0 (width - String.length s)) ' '
let code_char c = if Char.equal c '.' then ' ' else c

(* The pane stays decoupled from Git_data (whose folder-level dependency
   would be circular): the root maps its load state to this. [`Empty]
   carries the pane's own idle message ("nothing staged" vs "working tree
   clean"). *)
type status =
  [ `Loading
  | `Error of Error.t
  | `Empty of string
  | `Tree
  ]

(* One owner for the viewport mapping: render and the click handler must
   agree on it or clicks activate the wrong row. *)
let offset ~cursor ~height = Int.max 0 (cursor - height + 1)

(* Each pane shows only ITS side's letter — the staged pane the index
   letter, Changes the worktree letter (real chars as parsed: an add/add
   conflict reads its true code, not a flattened one). *)
let status_code ~with_sel ~side (entry : Git.Status.Entry.t) =
  let c, attrs =
    match side, entry.kind with
    | _, (Untracked | Unmerged) -> entry.worktree, Theme.untracked
    | `Staged, (Changed | Renamed _) -> entry.index, Theme.staged
    | `Unstaged, (Changed | Renamed _) -> entry.worktree, Theme.unstaged
  in
  seg (with_sel attrs) (String.of_char (code_char c))
;;

(* Row shape, lazygit-style: the whole row indents together, so the status
   code sits immediately beside its name instead of in a detached column. *)
let row ~width ~selected ~side (r : Tree.row) =
  let with_sel attrs = if selected then Theme.selection_bg :: attrs else attrs in
  let marker =
    if selected then seg (with_sel Theme.header) "\u{276F} " else seg (with_sel []) "  "
  in
  let indent depth = seg (with_sel []) (String.make (2 * depth) ' ') in
  match r with
  | Tree.Dir { name; depth; expanded; _ } ->
    let arrow = if expanded then "\u{25BE} " else "\u{25B8} " in
    (* arrow+space is 2 cells (String.length would overcount the glyph). *)
    let used = 2 + (2 * depth) + 2 + String.length name in
    View.hcat
      [ marker
      ; indent depth
      ; seg (with_sel Theme.header) (arrow ^ name)
      ; seg (with_sel []) (String.make (Int.max 0 (width - used)) ' ')
      ]
  | File { entry; name; depth } ->
    let label =
      match entry.kind with
      | Renamed { from } -> sprintf "%s \u{2190} %s" name from
      | Changed | Untracked | Unmerged -> name
    in
    View.hcat
      [ marker
      ; indent depth
      ; status_code ~with_sel ~side entry
      ; seg (with_sel []) " "
      ; seg
          (with_sel [ Attr.fg Theme.text ])
          (pad_to (Int.max 1 (width - 4 - (2 * depth))) label)
      ]
;;

let render ~(status : status) ~rows ~cursor ~side ~(dimensions : Dimensions.t) =
  match status with
  | `Loading -> seg Theme.context " loading git status..."
  | `Error e -> seg Theme.untracked (" git error: " ^ Error.to_string_hum e)
  | `Empty message -> seg Theme.context (" " ^ message)
  | `Tree ->
    let height = dimensions.height in
    let offset = offset ~cursor ~height in
    let bar = Scrollbar.view ~total:(List.length rows) ~visible:height ~offset in
    let row_width = dimensions.width - if Option.is_some bar then 1 else 0 in
    let body =
      View.vcat
        (List.take (List.drop rows offset) height
         |> List.mapi ~f:(fun i r -> row ~width:row_width ~selected:(i + offset = cursor) ~side r))
    in
    Scrollbar.attach ~width:dimensions.width ~bar body
;;
