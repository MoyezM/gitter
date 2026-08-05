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

(* Group lines at "diff --git" boundaries, then each file's tail at "@@"
   boundaries. The headerless leading groups — anything before the first
   file, and each file's ---/+++/index preamble — fall out of the match. *)
let parse output : File.t list =
  let starts prefix line = String.is_prefix line ~prefix in
  String.split_lines output
  |> List.group ~break:(fun _ line -> starts "diff --git" line)
  |> List.filter_map ~f:(function
    | header :: rest when starts "diff --git" header ->
      let hunks =
        List.group rest ~break:(fun _ line -> starts "@@" line)
        |> List.filter_map ~f:(function
          | hh :: body when starts "@@" hh ->
            let old_start, new_start = hunk_starts hh in
            Some
              { Hunk.header = hh
              ; old_start
              ; new_start
              ; lines = List.filter_map body ~f:body_line
              }
          | _ -> None)
      in
      Some { File.path = path_of_header header; hunks }
    | _ -> None)
;;
