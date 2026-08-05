open! Core
open! Bonsai_term

(* Pure View production for the branch-stack pane. The tree grows down:
   trunk first, children indented below (the parse orders it that way). *)

let seg attrs s = View.text ~attrs s

(* The pane stays decoupled from Git_data: the root maps its load state to
   this. *)
type status =
  [ `Loading
  | `Error of Error.t
  | `Empty of string
  | `Stack of Git.Branch_stack.Branch.t list
  ]

let row ~width ~selected (r : State.Row.t) =
  let with_sel attrs = if selected then Theme.selection_bg :: attrs else attrs in
  let fold =
    if r.has_children then (if r.collapsed then "\u{25B8} " else "\u{25BE} ") else "  "
  in
  let hidden = if r.hidden > 0 then sprintf " (+%d)" r.hidden else "" in
  let marker, name, name_attrs, restack =
    match r.kind with
    | State.Row.Group { prefix } -> "", prefix ^ "/", Theme.header, ""
    | Branch b ->
      let marker, attrs =
        if b.is_current
        then "\u{25C9} ", [ Attr.fg Theme.green; Attr.bold ]
        else if b.is_trunk
        then "\u{25EF} ", Theme.context
        else "\u{25EF} ", [ Attr.fg Theme.text ]
      in
      (* Restack churn on branches outside the current stack is noise — a
         work repo's stale branches all trail the trunk. *)
      let restack =
        if b.needs_restack && r.on_current_stack then " (needs restack)" else ""
      in
      marker, b.name, attrs, restack
  in
  let marker_cells = if String.is_empty marker then 0 else 2 in
  let indent = String.make (2 * r.depth) ' ' in
  let used =
    1 + (2 * r.depth) + 2 + marker_cells + String.length name + String.length hidden
    + String.length restack
  in
  View.hcat
    [ seg (with_sel []) (" " ^ indent ^ fold)
    ; seg (with_sel name_attrs) (marker ^ name)
    ; seg (with_sel Theme.context) hidden
    ; seg (with_sel [ Attr.fg Theme.yellow ]) restack
    ; seg (with_sel []) (String.make (Int.max 0 (width - used)) ' ')
    ]
;;

let render ~(status : status) ~(model : State.Model.t) ~(dimensions : Dimensions.t) =
  match status with
  | `Loading -> seg Theme.context " loading branches..."
  | `Error e -> seg Theme.untracked (" git error: " ^ Error.to_string_hum e)
  | `Empty message -> seg Theme.context (" " ^ message)
  | `Stack branches ->
    let rows = State.visible ~branches ~overrides:model.overrides in
    let height = dimensions.height in
    let offset = State.offset ~total:(List.length rows) ~height model.scroll in
    let cursor = State.index_of rows model.selection in
    let bar = Scrollbar.view ~total:(List.length rows) ~visible:height ~offset in
    let row_width = dimensions.width - if Option.is_some bar then 1 else 0 in
    let body =
      View.vcat
        (List.take (List.drop rows offset) height
         |> List.mapi ~f:(fun i r -> row ~width:row_width ~selected:(i + offset = cursor) r))
    in
    Scrollbar.attach ~width:dimensions.width ~bar body
;;
