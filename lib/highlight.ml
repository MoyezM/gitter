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

type session =
  { starts : int array (* byte offset of each line start *)
  ; length : int
  ; tree : Tree_sitter.Tree.t
  ; query : Tree_sitter.Query.t
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
    ( language ()
    , Hashtbl.find_or_add query_cache ext ~default:(fun () ->
        Tree_sitter.Query.create (language ()) ~source:highlights_query) ))
;;

let build ~language ~query content =
  try
    let parser = Tree_sitter.Parser.create language in
    let tree = Tree_sitter.Parser.parse_string parser content in
    Some { starts = line_starts content; length = String.length content; tree; query }
  with
  | _ -> None
;;

(* Synchronous create: parses on the calling domain. For tests/benchmarks. *)
let create_sync ~path content : t =
  Option.bind (session_spec ~path) ~f:(fun (language, query) ->
    build ~language ~query content)
;;

(* Async create: the parse runs in its own domain via [Cpu.in_domain] — the
   C stubs never release the runtime lock, so parsing on the scheduler would
   stall every frame. (The try covers [Domain.spawn] itself, e.g. domain
   exhaustion: highlighting failure must never break the diff view.) *)
let create ~path content : t Async.Deferred.t =
  match session_spec ~path with
  | None -> Async.return None
  | Some (language, query) ->
    (try Cpu.in_domain (fun () -> build ~language ~query content) with
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
      let triples =
        try Tree_sitter.highlight_range s.query s.tree ~start_byte ~end_byte with
        | _ -> []
      in
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
