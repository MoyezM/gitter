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

(* The (file, hunk) the row at [row] belongs to: counts headers on the way
   down. None on rows above the first hunk. Documents without File_header
   rows (single-file diffs) are file 0. *)
let hunk_at (t : t) ~row =
  let row = Int.min row (Array.length t - 1) in
  if row < 0
  then None
  else (
    let file = ref 0
    and seen_file = ref false
    and hunk = ref (-1) in
    for i = 0 to row do
      match t.(i) with
      | File_header _ ->
        if !seen_file then incr file;
        seen_file := true;
        hunk := -1
      | Hunk_header _ -> incr hunk
      | Diff_line _ -> ()
    done;
    if !hunk < 0 then None else Some (!file, !hunk))
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
