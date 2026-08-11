open! Core

(* Syntax highlighting, helix-style: parse the file ONCE into a tree-sitter
   tree, then run the highlight query only over the lines being rendered
   ([window]) — never over the whole file.

   Benchmarks that shaped this (bench/bench_diff.ml, 3.9MB generated C):
   - full-file query: 238ms + 130ms slicing  -> viewport query: 0.3ms
   - parse: 262ms and the stubs hold the OCaml runtime lock, so [create]
     parses in a SEPARATE DOMAIN and rendering never blocks; use
     [create_sync] only off the UI (tests, benchmarks).

   The channel convention stands: syntax owns the foreground, diff status
   owns the background. *)

module Span = struct
  type t =
    { start_col : int (* byte columns within the line *)
    ; end_col : int
    ; capture : string
    }
end

(* An embedded language inside the file: a byte range, plus its own tree and
   query. The tree is parsed with [set_included_ranges], so its node offsets
   are in the PARENT file's coordinates and need no translation. *)
type injection =
  { start_byte : int
  ; end_byte : int
  ; tree : Tree_sitter.Tree.t
  ; query : Tree_sitter.Query.t
  }

type session =
  { starts : int array (* byte offset of each line start *)
  ; length : int
  ; tree : Tree_sitter.Tree.t
  ; query : Tree_sitter.Query.t
  ; injections : injection list (* disjoint, ascending by start_byte *)
  }

type t = session option

let empty : t = None
let is_empty = Option.is_none

let line_starts content =
  String.foldi content ~init:[ 0 ] ~f:(fun i acc c ->
    if Char.equal c '\n' then (i + 1) :: acc else acc)
  |> List.rev
  |> Array.of_list
;;

(* Query objects are cheap (~2ms) but per-language, not per-file. The cache
   only runs on the main domain. *)
let query_cache : (string, Tree_sitter.Query.t) Hashtbl.t = Hashtbl.create (module String)

(* The one owner of "path -> (language, query)": extension split, spec
   lookup, query cache. None means "don't highlight" — including when the
   grammar or query fails to load. *)
let session_spec ~path =
  let open Option.Let_syntax in
  let%bind ext = snd (Filename.split_extension path) in
  let%bind { Grammar_registry.language; highlights_query } = Grammar.find ext in
  Option.try_with (fun () ->
    ( ext
    , language ()
    , Hashtbl.find_or_add query_cache ext ~default:(fun () ->
        Tree_sitter.Query.create (language ()) ~source:highlights_query) ))
;;

(* Children of [node] whose kind is [kind]. Direct children only: the rule's
   nodes sit immediately under the block. *)
let child_of_kind node ~kind =
  let n = Tree_sitter.Node.child_count node in
  let rec go i =
    if i >= n
    then None
    else (
      match Tree_sitter.Node.child node i with
      | Some c when String.equal (Tree_sitter.Node.kind c) kind -> Some c
      | _ -> go (i + 1))
  in
  go 0
;;

let node_text content node =
  let s = Tree_sitter.Node.start_byte node in
  let e = Tree_sitter.Node.end_byte node in
  if s >= 0 && e <= String.length content && e > s then String.sub content ~pos:s ~len:(e - s) else ""
;;

(* Every node of [tree], parents before children. Iterative on purpose: depth
   is set by the document, which is untrusted input, and native-code
   [Stack_overflow] is fatal rather than catchable. Children are pushed in
   reverse so they pop in document order. *)
let iter_nodes tree ~f =
  let stack = ref [ Tree_sitter.Tree.root_node tree ] in
  while not (List.is_empty !stack) do
    match !stack with
    | [] -> ()
    | node :: rest ->
      stack := rest;
      f node;
      for i = Tree_sitter.Node.child_count node - 1 downto 0 do
        match Tree_sitter.Node.child node i with
        | Some c -> stack := c :: !stack
        | None -> ()
      done
  done
;;

(* Parse only [range] of [content], with [content] passed WHOLE. That is the
   point of [set_included_ranges]: tree-sitter parses just the range, but the
   resulting nodes carry offsets in the parent file's coordinates, so nothing
   downstream has to translate them. Slicing the substring instead would mean
   re-basing every offset by hand. *)
let parse_range ~lang ~content range =
  let parser = Tree_sitter.Parser.create lang in
  Tree_sitter.Parser.set_included_ranges parser [| range |];
  Tree_sitter.Parser.parse_string parser content
;;

(* The injection for one block node, if it names a language we have a grammar
   for. None covers all the ordinary misses: an unlabelled fence (no info
   child), an unknown language, and — via [try_with] — a grammar or query
   that fails to load, which must cost this block only, not the file.

   [queries] is keyed by EXTENSION. [Language.name] is "" for every vendored
   grammar, so keying on it collapses the cache to a single entry and hands
   the first language's query to every later block: valid node positions,
   wrong capture names. *)
let injection_of_block ~rule ~content ~queries node =
  let open Option.Let_syntax in
  let%bind info_node = child_of_kind node ~kind:rule.Grammar.info in
  let%bind code_node = child_of_kind node ~kind:rule.Grammar.content in
  let%bind ext, { Grammar_registry.language; highlights_query } =
    Grammar.find_embedded (node_text content info_node)
  in
  Option.try_with (fun () ->
    let lang = language () in
    let query =
      Hashtbl.find_or_add queries ext ~default:(fun () ->
        Tree_sitter.Query.create lang ~source:highlights_query)
    in
    let range =
      { Tree_sitter.start_byte = Tree_sitter.Node.start_byte code_node
      ; end_byte = Tree_sitter.Node.end_byte code_node
      ; start_point = Tree_sitter.Node.start_point code_node
      ; end_point = Tree_sitter.Node.end_point code_node
      }
    in
    { start_byte = range.Tree_sitter.start_byte
    ; end_byte = range.Tree_sitter.end_byte
    ; tree = parse_range ~lang ~content range
    ; query
    })
;;

(* Embedded-language regions: every block node the rule names, resolved to an
   injection. Queries are cached PER BUILD, not in [query_cache] — this runs
   in a separate domain (see [create]) and that cache is main-domain only. *)
let injections ~rule ~content tree =
  let queries = Hashtbl.create (module String) in
  let out = ref [] in
  iter_nodes tree ~f:(fun node ->
    if String.equal (Tree_sitter.Node.kind node) rule.Grammar.block
    then
      Option.iter (injection_of_block ~rule ~content ~queries node) ~f:(fun i ->
        out := i :: !out));
  List.sort !out ~compare:(fun a b -> Int.compare a.start_byte b.start_byte)
;;

let build ~ext ~language ~query content =
  try
    let parser = Tree_sitter.Parser.create language in
    let tree = Tree_sitter.Parser.parse_string parser content in
    let injections =
      match Grammar.injection_rule ext with
      | None -> []
      | Some rule -> (try injections ~rule ~content tree with _ -> [])
    in
    Some
      { starts = line_starts content
      ; length = String.length content
      ; tree
      ; query
      ; injections
      }
  with
  | _ -> None
;;

(* Synchronous create: parses on the calling domain. For tests/benchmarks. *)
let create_sync ~path content : t =
  Option.bind (session_spec ~path) ~f:(fun (ext, language, query) ->
    build ~ext ~language ~query content)
;;

(* Async create: the parse runs in its own domain via [Cpu.in_domain] — the
   C stubs never release the runtime lock, so parsing on the scheduler would
   stall every frame. (The try covers [Domain.spawn] itself, e.g. domain
   exhaustion: highlighting failure must never break the diff view.) *)
let create ~path content : t Async.Deferred.t =
  match session_spec ~path with
  | None -> Async.return None
  | Some (ext, language, query) ->
    (try Cpu.in_domain (fun () -> build ~ext ~language ~query content) with
     | _ -> Async.return None)
;;

let line_end t i = if i + 1 < Array.length t.starts then t.starts.(i + 1) - 1 else t.length

let line_of ~starts byte =
  match
    Array.binary_search starts `Last_less_than_or_equal_to byte ~compare:Int.compare
  with
  | Some i -> i
  | None -> 0
;;

(* A window owns its origin: callers index by ABSOLUTE 1-based line number
   and cannot desync from the range the window was built over. *)
module Window = struct
  type t =
    { from_line : int
    ; spans : Span.t list array
    }

  let empty = { from_line = 1; spans = [||] }

  let line t line =
    let i = line - t.from_line in
    if i >= 0 && i < Array.length t.spans then t.spans.(i) else []
  ;;
end

(* Spans for lines [from_line .. to_line] (1-based, inclusive): ONE ranged
   query over just those bytes, distributed per line. *)
let window (t : t) ~from_line ~to_line : Window.t =
  match t with
  | None -> Window.empty
  | Some s ->
    let n_lines = Array.length s.starts in
    let from_line = Int.max 1 from_line in
    let to_line = Int.min n_lines to_line in
    if from_line > to_line
    then Window.empty
    else (
      let start_byte = s.starts.(from_line - 1) in
      let end_byte = line_end s (to_line - 1) in
      let live =
        List.filter s.injections ~f:(fun i ->
          i.end_byte > start_byte && i.start_byte < end_byte)
      in
      let parent =
        try Tree_sitter.highlight_range s.query s.tree ~start_byte ~end_byte with
        | _ -> []
      in
      (* The parent grammar captures the whole block — markdown tags a
         [fenced_code_block] @text.literal, fences and body alike — and the
         renderer resolves an overlap in favour of the span that STARTS
         first, which is always the parent's. So cut the injected ranges out
         of the parent's captures: the fence delimiters keep their markdown
         styling, the code between them belongs to its own grammar. *)
      let parent =
        if List.is_empty live
        then parent
        else
          List.concat_map parent ~f:(fun (sb, eb, capture) ->
            List.fold live ~init:[ sb, eb ] ~f:(fun pieces i ->
              List.concat_map pieces ~f:(fun (ps, pe) ->
                if i.end_byte <= ps || i.start_byte >= pe
                then [ ps, pe ]
                else
                  (if ps < i.start_byte then [ ps, i.start_byte ] else [])
                  @ if i.end_byte < pe then [ i.end_byte, pe ] else []))
            |> List.map ~f:(fun (s, e) -> s, e, capture))
      in
      let injected =
        List.concat_map live ~f:(fun i ->
          try
            Tree_sitter.highlight_range
              i.query
              i.tree
              ~start_byte:(Int.max start_byte i.start_byte)
              ~end_byte:(Int.min end_byte i.end_byte)
          with
          | _ -> [])
      in
      let triples = parent @ injected in
      let out = Array.create ~len:(to_line - from_line + 1) [] in
      (* Distribute each capture to the lines it touches: the affected line
         range is two [line_of] lookups, per-line columns are clamps. The
         ranged query can return captures reaching far outside the window
         (e.g. a file-head comment) — clamping to the window, rather than
         walking from the capture's start line, keeps this O(window). *)
      List.iter triples ~f:(fun (sb, eb, capture) ->
        let first = Int.max from_line (line_of ~starts:s.starts sb + 1) in
        let last = Int.min to_line (line_of ~starts:s.starts (eb - 1) + 1) in
        for line = first to last do
          let i = line - 1 in
          let ls = s.starts.(i) in
          let start_col = Int.max sb ls - ls in
          let end_col = Int.min eb (line_end s i) - ls in
          if end_col > start_col
          then
            out.(line - from_line)
            <- { Span.start_col; end_col; capture } :: out.(line - from_line)
        done);
      let spans =
        Array.map out ~f:(fun spans ->
          List.sort spans ~compare:(fun a b -> Int.compare a.Span.start_col b.Span.start_col))
      in
      { Window.from_line; spans })
;;
