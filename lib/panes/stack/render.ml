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

let row ~width ~base ~selected ~query (r : State.Row.t) =
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
      let restack =
        match base with
        | Some base when String.equal base b.name -> " (base)" ^ restack
        | _ -> restack
      in
      marker, b.name, attrs, restack
  in
  (* Only branch rows are match candidates (T1); underline is the match
     channel (T3, R3). *)
  let name_segs =
    let attrs = with_sel name_attrs in
    match r.kind with
    | State.Row.Branch b ->
      seg attrs marker
      :: Search.Tree_search.underline ~query ~attrs ~candidate:b.name name
    | Group _ -> [ seg attrs (marker ^ name) ]
  in
  let marker_cells = if String.is_empty marker then 0 else 2 in
  let indent = String.make (2 * r.depth) ' ' in
  let used =
    1 + (2 * r.depth) + 2 + marker_cells + String.length name + String.length hidden
    + String.length restack
  in
  View.hcat
    ((seg (with_sel []) (" " ^ indent ^ fold) :: name_segs)
     @ [ seg (with_sel Theme.context) hidden
       ; seg (with_sel [ Attr.fg Theme.yellow ]) restack
       ; seg (with_sel []) (String.make (Int.max 0 (width - used)) ' ')
       ])
;;

(* [rows] is the host's shared visible-rows derivation — render must not
   rebuild the forest itself. *)
let render ~(status : status) ~rows ~(model : State.Model.t) ~base ~(dimensions : Dimensions.t)
  =
  match status with
  | `Loading -> seg Theme.context " loading branches..."
  | `Error e -> seg Theme.untracked (" git error: " ^ Error.to_string_hum e)
  | `Empty message -> seg Theme.context (" " ^ message)
  | `Stack (_ : Git.Branch_stack.Branch.t list) ->
    let search = model.Search.Tree_search.Model.search in
    List_view.render
      ~rows
      ~scroll:(State.scroll model)
      ~cursor:(State.index_of rows (State.selection_key model))
      ~dimensions
      ~row:
        ((* Parse the query ONCE per render — [underline] runs per row. *)
         let query = Search.Prompt.parsed search in
         fun ~selected ~width r -> row ~width ~base ~selected ~query r)
;;
