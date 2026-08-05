open! Core

type line =
  | File_header of string
  | Hunk_header of string
  | Diff_line of int option * int option * Git.Diff.Line.t

type t = line array

let is_diff_line = function
  | Diff_line _ -> true
  | File_header _ | Hunk_header _ -> false
;;

(* A File_header row per file is only worth showing when several files are
   present (relative mode); the uncommitted view shows one file per fetch. *)
let of_files files : t =
  let many = List.length files > 1 in
  List.concat_map files ~f:(fun (file : Git.Diff.File.t) ->
    (if many then [ File_header file.path ] else [])
    @ List.concat_map file.hunks ~f:(fun hunk ->
      Hunk_header hunk.header
      :: List.map (Git.Diff.Hunk.numbered hunk) ~f:(fun (o, n, l) -> Diff_line (o, n, l))))
  |> Array.of_list
;;
