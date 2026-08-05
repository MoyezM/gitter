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

(* A hunk-body line; None drops "\ No newline at end of file" and junk. *)
let body_line line =
  match String.prefix line 1 with
  | "+" -> Some (Line.Added (String.drop_prefix line 1))
  | "-" -> Some (Line.Removed (String.drop_prefix line 1))
  | " " -> Some (Line.Context (String.drop_prefix line 1))
  | _ -> None
;;

(* Split [lines] at each line beginning with [marker]:

     split_runs ~marker:"@@" ["index"; "@@ -1 +1 @@"; "+a"; "@@ -9 +9 @@"]
     = ~leading:["index"], ~runs:[ "@@ -1 +1 @@", ["+a"]; "@@ -9 +9 @@", [] ]

   Every run is a marker line paired with the lines under it; [leading] is
   whatever preceded the first marker. *)
let split_runs ~marker lines =
  let is_marker line = String.is_prefix line ~prefix:marker in
  let groups = List.group lines ~break:(fun _ line -> is_marker line) in
  let leading, run_groups =
    match groups with
    | (line :: _ as first) :: rest when not (is_marker line) -> first, rest
    | groups -> [], groups
  in
  let runs =
    List.filter_map run_groups ~f:(function
      | header :: body -> Some (header, body)
      | [] -> None)
  in
  ~leading, ~runs
;;

let hunk_of_run (header, body) =
  let old_start, new_start = hunk_starts header in
  { Hunk.header; old_start; new_start; lines = List.filter_map body ~f:body_line }
;;

let file_of_run (header, rest) =
  let ~leading:_preamble, ~runs = split_runs ~marker:"@@" rest in
  { File.path = path_of_header header; hunks = List.map runs ~f:hunk_of_run }
;;

let parse output : File.t list =
  let ~leading:_junk, ~runs = split_runs ~marker:"diff --git" (String.split_lines output) in
  List.map runs ~f:file_of_run
;;
