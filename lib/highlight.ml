open! Core

(* Full-file syntax highlighting, sliced per line for diff rendering.

   The channel convention (delta/GitHub): syntax owns the FOREGROUND, diff
   status owns the BACKGROUND — so highlights compose with row tints.

   Language registry: adding a language is one vendored grammar library
   (see grammars/python/ for the pattern) plus one entry here. *)

module Span = struct
  type t =
    { start_col : int (* byte columns within the line *)
    ; end_col : int
    ; capture : string
    }
  [@@deriving sexp, equal]
end

(* Per-line capture spans; line numbers are 1-based via [line]. *)
type t = Span.t list array

let empty : t = [||]

(* Built-ins ship inside the tree-sitter opam package; everything else comes
   from the generated registry (see scripts/add-grammar — adding a language
   never touches this file). *)
let builtin = function
  | "ml" -> Some Tree_sitter_ocaml.highlight_ocaml
  | "mli" -> Some Tree_sitter_ocaml.highlight_interface
  | "json" -> Some Tree_sitter_json.highlight
  | _ -> None
;;

let highlighter_for_path path =
  match snd (Filename.split_extension path) with
  | None -> None
  | Some ext ->
    (match builtin ext with
     | Some h -> Some h
     | None -> Grammar_registry.find ext)
;;

let line_starts content =
  let starts = ref [ 0 ] in
  String.iteri content ~f:(fun i c -> if Char.equal c '\n' then starts := (i + 1) :: !starts);
  Array.of_list (List.rev !starts)
;;

let line_of ~starts byte =
  match
    Array.binary_search starts `Last_less_than_or_equal_to byte ~compare:Int.compare
  with
  | Some i -> i
  | None -> 0
;;

(* Split byte-range captures at line boundaries into per-line column spans. *)
let slice ~content triples : t =
  let starts = line_starts content in
  let n_lines = Array.length starts in
  let per_line = Array.create ~len:n_lines [] in
  let line_end i = if i + 1 < n_lines then starts.(i + 1) - 1 else String.length content in
  List.iter triples ~f:(fun (start_byte, end_byte, capture) ->
    let rec go pos i =
      if pos < end_byte && i < n_lines
      then (
        let stop = Int.min end_byte (line_end i) in
        if stop > pos
        then
          per_line.(i)
          <- { Span.start_col = pos - starts.(i); end_col = stop - starts.(i); capture }
             :: per_line.(i);
        go (if i + 1 < n_lines then starts.(i + 1) else end_byte) (i + 1))
    in
    go start_byte (line_of ~starts start_byte));
  Array.map per_line ~f:(fun spans ->
    List.sort spans ~compare:(fun a b -> Int.compare a.Span.start_col b.Span.start_col))
;;

let of_content ~path content : t =
  match highlighter_for_path path with
  | None -> empty
  | Some highlight ->
    (try slice ~content (highlight content) with
     | _ -> empty)
;;

let line (t : t) n = if n >= 1 && n <= Array.length t then t.(n - 1) else []
