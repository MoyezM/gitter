open! Core

(* Parser for unified diff output ([git diff --no-color]). Pure, unit
   testable. Structure: files -> hunks -> lines. *)

module Line = struct
  type t =
    | Context of string
    | Added of string
    | Removed of string
  [@@deriving sexp, equal]
end

module Hunk = struct
  type t =
    { header : string (* the whole @@ line *)
    ; lines : Line.t list
    }
  [@@deriving sexp, equal]
end

module File = struct
  type t =
    { path : string (* the post-image path *)
    ; hunks : Hunk.t list
    }
  [@@deriving sexp, equal]
end

(* "diff --git a/foo b/foo" -> "foo". Quoted/renamed paths keep the b-side
   verbatim minus the "b/" prefix. *)
let path_of_header line =
  match String.rsplit2 line ~on:' ' with
  | Some (_, b_path) -> String.chop_prefix b_path ~prefix:"b/" |> Option.value ~default:b_path
  | None -> line
;;

type acc =
  { files : File.t list
  ; path : string option
  ; hunks : Hunk.t list
  ; header : string option
  ; lines : Line.t list
  }

let flush_hunk acc =
  match acc.header with
  | None -> acc
  | Some header ->
    { acc with
      hunks = acc.hunks @ [ { Hunk.header; lines = acc.lines } ]
    ; header = None
    ; lines = []
    }
;;

let flush_file acc =
  let acc = flush_hunk acc in
  match acc.path with
  | None -> acc
  | Some path -> { acc with files = acc.files @ [ { File.path; hunks = acc.hunks } ]; path = None; hunks = [] }
;;

let parse output : File.t list =
  let step acc line =
    if String.is_prefix line ~prefix:"diff --git"
    then { (flush_file acc) with path = Some (path_of_header line) }
    else if String.is_prefix line ~prefix:"@@"
    then { (flush_hunk acc) with header = Some line }
    else (
      match acc.header with
      | None -> acc (* file headers: ---, +++, index, mode, etc. *)
      | Some _ ->
        let add l = { acc with lines = acc.lines @ [ l ] } in
        (match String.prefix line 1 with
         | "+" -> add (Added (String.drop_prefix line 1))
         | "-" -> add (Removed (String.drop_prefix line 1))
         | " " -> add (Context (String.drop_prefix line 1))
         | "\\" -> acc (* "\ No newline at end of file" *)
         | _ -> acc))
  in
  let init = { files = []; path = None; hunks = []; header = None; lines = [] } in
  let final = flush_file (List.fold (String.split_lines output) ~init ~f:step) in
  final.files
;;
