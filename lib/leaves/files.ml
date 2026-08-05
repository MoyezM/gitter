open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The Files leaf: renders the app-root git status (flat list, two-column
   git status codes) with a cursor. All data and the cursor live in
   [Git_data]; this leaf is presentation plus event forwarding. *)

let seg attrs s = View.text ~attrs s
let pad_to width s = s ^ String.make (Int.max 0 (width - String.length s)) ' '

let code_char c = if Char.equal c '.' then ' ' else c

(* The parser stores the real XY letters, so print them as-is (an add/add
   conflict reads "AA", not a flattened "UU"); kind only picks the colors. *)
let status_code ~with_sel (entry : Git.Status.Entry.t) =
  let index_attrs, worktree_attrs =
    match entry.kind with
    | Untracked | Unmerged -> Theme.untracked, Theme.untracked
    | Changed | Renamed _ -> Theme.staged, Theme.unstaged
  in
  [ seg (with_sel index_attrs) (String.of_char (code_char entry.index))
  ; seg (with_sel worktree_attrs) (String.of_char (code_char entry.worktree))
  ]
;;

let row ~width ~selected (entry : Git.Status.Entry.t) =
  let with_sel attrs = if selected then Theme.selection_bg :: attrs else attrs in
  let marker =
    if selected then seg (with_sel Theme.header) "\u{276F} " else seg (with_sel []) "  "
  in
  let label =
    match entry.kind with
    | Renamed { from } -> sprintf "%s \u{2190} %s" entry.path from
    | Changed | Untracked | Unmerged -> entry.path
  in
  let name = seg (with_sel [ Attr.fg Theme.text ]) (pad_to (Int.max 1 (width - 5)) label) in
  View.hcat (marker :: (status_code ~with_sel entry @ [ seg (with_sel []) " "; name ]))
;;

let render ~(load : Git_data.Load.t) ~cursor ~(dimensions : Dimensions.t) =
  match load with
  | Not_loaded | Loading -> seg Theme.context " loading git status..."
  | Loaded (Error e) -> seg Theme.untracked (" git error: " ^ Error.to_string_hum e)
  | Loaded (Ok []) -> seg Theme.context " working tree clean"
  | Loaded (Ok entries) ->
    let offset = Int.max 0 (cursor - dimensions.height + 1) in
    let rows =
      List.take (List.drop entries offset) dimensions.height
      |> List.mapi ~f:(fun i entry ->
        row ~width:dimensions.width ~selected:(i + offset = cursor) entry)
    in
    View.vcat rows
;;

let component ~(data : Git_data.t) : Widget.t =
  fun ~dimensions (local_ _graph) ->
  let view =
    let%arr load = data.load
    and cursor = data.cursor
    and dimensions in
    render ~load ~cursor ~dimensions
  in
  let handler =
    let%arr inject = data.files_inject
    and cursor = data.cursor
    and dimensions in
    fun (event : Event.t) ->
      let offset = Int.max 0 (cursor - dimensions.height + 1) in
      match event with
      | Event.Key_press { key = ASCII 'j'; mods = [] }
      | Key_press { key = Arrow `Down; mods = [] } -> inject (Move `Down)
      | Key_press { key = ASCII 'k'; mods = [] }
      | Key_press { key = Arrow `Up; mods = [] } -> inject (Move `Up)
      | Mouse { kind = Scroll `Down; _ } -> inject (Move `Down)
      | Mouse { kind = Scroll `Up; _ } -> inject (Move `Up)
      | Mouse { kind = Left; position; _ } -> inject (Set (position.y + offset))
      | _ -> Effect.Ignore
  in
  ~view, ~handler
;;
