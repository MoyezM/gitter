open! Core

(* Parser for unified diff output ([git diff --no-color]). Pure, unit
   testable. Structure: files -> hunks -> lines. *)

module Line = struct
  type t =
    | Context of string
    | Added of string
    | Removed of string
  [@@deriving equal]
end

module Hunk = struct
  type t =
    { header : string (* the whole @@ line *)
    ; old_start : int (* first line number on the pre-image side *)
    ; new_start : int (* first line number on the post-image side *)
    ; lines : Line.t list
    }

  (* Each line paired with its (old, new) line numbers; the side a line
     doesn't exist on is [None]. *)
  let numbered t =
    List.folding_map t.lines ~init:(t.old_start, t.new_start) ~f:(fun (o, n) line ->
      match line with
      | Line.Context _ -> (o + 1, n + 1), (Some o, Some n, line)
      | Added _ -> (o, n + 1), (None, Some n, line)
      | Removed _ -> (o + 1, n), (Some o, None, line))
  ;;
end

module File = struct
  type t =
    { path : string (* the post-image path *)
    ; hunks : Hunk.t list
    }
end

(* "diff --git a/foo b/foo" -> "foo". Quoted/renamed paths keep the b-side
   verbatim minus the "b/" prefix. *)
let path_of_header line =
  match String.rsplit2 line ~on:' ' with
  | Some (_, b_path) -> String.chop_prefix b_path ~prefix:"b/" |> Option.value ~default:b_path
  | None -> line
;;

(* "@@ -13,22 +13,17 @@ ..." -> (13, 13). Counts are optional ("-13 +13"). *)
let hunk_starts header =
  let start_of token =
    String.drop_prefix token 1 (* the sign *)
    |> String.lsplit2 ~on:','
    |> Option.value_map ~default:(String.drop_prefix token 1) ~f:fst
    |> Int.of_string_opt
    |> Option.value ~default:0
  in
  match String.split header ~on:' ' with
  | _at :: old_tok :: new_tok :: _ -> start_of old_tok, start_of new_tok
  | _ -> 0, 0
;;

(* Accumulator lists are kept REVERSED and only reversed at flush — appending
   with [@] is quadratic, which on a 100k-line diff (a vendored parser.c...)
   freezes the whole UI since parsing runs on the render scheduler. *)
type acc =
  { files : File.t list (* reversed *)
  ; path : string option
  ; hunks : Hunk.t list (* reversed *)
  ; header : string option
  ; lines : Line.t list (* reversed *)
  }

let flush_hunk acc =
  match acc.header with
  | None -> acc
  | Some header ->
    let old_start, new_start = hunk_starts header in
    { acc with
      hunks =
        { Hunk.header; old_start; new_start; lines = List.rev acc.lines } :: acc.hunks
    ; header = None
    ; lines = []
    }
;;

let flush_file acc =
  let acc = flush_hunk acc in
  match acc.path with
  | None -> acc
  | Some path ->
    { acc with
      files = { File.path; hunks = List.rev acc.hunks } :: acc.files
    ; path = None
    ; hunks = []
    }
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
        let add l = { acc with lines = l :: acc.lines } in
        (match String.prefix line 1 with
         | "+" -> add (Added (String.drop_prefix line 1))
         | "-" -> add (Removed (String.drop_prefix line 1))
         | " " -> add (Context (String.drop_prefix line 1))
         | "\\" -> acc (* "\ No newline at end of file" *)
         | _ -> acc))
  in
  let init = { files = []; path = None; hunks = []; header = None; lines = [] } in
  let final = flush_file (List.fold (String.split_lines output) ~init ~f:step) in
  List.rev final.files
;;
