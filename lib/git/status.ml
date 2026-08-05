open! Core

(* Parser for [git status --porcelain=v2] — the machine-readable format with
   a stability guarantee. Pure string -> types, so it's unit testable.

   Format reference (git-status(1)):
     1 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <path>                 changed
     2 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <X><score> <path>\t<origPath>
     u <XY> <sub> <m1> <m2> <m3> <mW> <h1> <h2> <h3> <path>       unmerged
     ? <path>                                                     untracked
     # ...                                                        headers

   [index]/[worktree] are the raw XY letters ('.' means unmodified). We keep
   them as chars — the UI renders them directly.

   Caveat: fields are space-separated with the path as the tail, so paths
   containing consecutive spaces would parse wrong; the -z variant fixes
   that if it ever matters. *)

module Entry = struct
  module Kind = struct
    type t =
      | Changed
      | Renamed of { from : string }
      | Untracked
      | Unmerged
    [@@deriving equal]
  end

  type t =
    { index : char (* status letter for the index side *)
    ; worktree : char (* status letter for the worktree side *)
    ; path : string
    ; kind : Kind.t
    }
end

(* The path is everything after the first [n] space-separated fields. *)
let path_after_fields line ~n =
  let rec skip pos remaining =
    if remaining = 0
    then Some (String.subo line ~pos)
    else (
      match String.index_from line pos ' ' with
      | Some i -> skip (i + 1) (remaining - 1)
      | None -> None)
  in
  skip 0 n
;;

(* XY sits at fixed bytes 2-3 on "1"/"2"/"u" lines: tag, space, XY. *)
let xy line = if String.length line > 3 then Some (line.[2], line.[3]) else None

let parse_line line : Entry.t option =
  match String.prefix line 1 with
  | "1" ->
    Option.both (xy line) (path_after_fields line ~n:8)
    |> Option.map ~f:(fun ((index, worktree), path) ->
      { Entry.index; worktree; path; kind = Changed })
  | "2" ->
    Option.both (xy line) (path_after_fields line ~n:9)
    |> Option.bind ~f:(fun ((index, worktree), tail) ->
      match String.lsplit2 tail ~on:'\t' with
      | Some (path, orig) ->
        Some { Entry.index; worktree; path; kind = Renamed { from = orig } }
      | None -> Some { Entry.index; worktree; path = tail; kind = Changed })
  | "u" ->
    Option.both (xy line) (path_after_fields line ~n:10)
    |> Option.map ~f:(fun ((index, worktree), path) ->
      { Entry.index; worktree; path; kind = Unmerged })
  | "?" ->
    path_after_fields line ~n:1
    |> Option.map ~f:(fun path ->
      { Entry.index = '?'; worktree = '?'; path; kind = Untracked })
  | _ -> None (* headers, ignored lines *)
;;

let parse output =
  String.split_lines output |> List.filter_map ~f:parse_line
;;
