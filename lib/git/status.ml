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

(* git C-quotes paths containing special bytes (core.quotePath, on by
   default for non-ASCII): the path arrives wrapped in double-quote chars
   with backslash escapes — quote, backslash, t, n, r, and 3-digit octal.
   Downstream git commands need the REAL bytes, so unquote at the parsing
   edge. *)
let unquote path =
  let n = String.length path in
  if n < 2 || (not (Char.equal path.[0] '"')) || not (Char.equal path.[n - 1] '"')
  then path
  else (
    let body = String.sub path ~pos:1 ~len:(n - 2) in
    let buf = Buffer.create (String.length body) in
    (* Three octal digits -> byte; None on malformed input (never produced
       by git, but the parser must be total). *)
    let octal i =
      let digit j =
        match body.[j] with
        | '0' .. '7' as c -> Some (Char.to_int c - Char.to_int '0')
        | _ -> None
      in
      match digit i, digit (i + 1), digit (i + 2) with
      | Some a, Some b, Some c ->
        let v = (a * 64) + (b * 8) + c in
        if v < 256 then Some (Char.of_int_exn v) else None
      | _ -> None
    in
    let rec go i =
      if i >= String.length body
      then ()
      else if Char.equal body.[i] '\\' && i + 1 < String.length body
      then (
        match body.[i + 1] with
        | 'a' -> Buffer.add_char buf '\007'; go (i + 2)
        | 'b' -> Buffer.add_char buf '\b'; go (i + 2)
        | 't' -> Buffer.add_char buf '\t'; go (i + 2)
        | 'n' -> Buffer.add_char buf '\n'; go (i + 2)
        | 'v' -> Buffer.add_char buf '\011'; go (i + 2)
        | 'f' -> Buffer.add_char buf '\012'; go (i + 2)
        | 'r' -> Buffer.add_char buf '\r'; go (i + 2)
        | '0' .. '7' when i + 3 < String.length body ->
          (match octal (i + 1) with
           | Some byte -> Buffer.add_char buf byte; go (i + 4)
           | None -> Buffer.add_char buf body.[i + 1]; go (i + 2))
        | c ->
          (* escaped quote, backslash, and anything else: the char itself *)
          Buffer.add_char buf c;
          go (i + 2))
      else (
        Buffer.add_char buf body.[i];
        go (i + 1))
    in
    go 0;
    Buffer.contents buf)
;;

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
      { Entry.index; worktree; path = unquote path; kind = Changed })
  | "2" ->
    Option.both (xy line) (path_after_fields line ~n:9)
    |> Option.bind ~f:(fun ((index, worktree), tail) ->
      match String.lsplit2 tail ~on:'\t' with
      | Some (path, orig) ->
        Some
          { Entry.index
          ; worktree
          ; path = unquote path
          ; kind = Renamed { from = unquote orig }
          }
      | None -> Some { Entry.index; worktree; path = unquote tail; kind = Changed })
  | "u" ->
    Option.both (xy line) (path_after_fields line ~n:10)
    |> Option.map ~f:(fun ((index, worktree), path) ->
      { Entry.index; worktree; path = unquote path; kind = Unmerged })
  | "?" ->
    path_after_fields line ~n:1
    |> Option.map ~f:(fun path ->
      { Entry.index = '?'; worktree = '?'; path = unquote path; kind = Untracked })
  | _ -> None (* headers, ignored lines *)
;;

let parse output =
  String.split_lines output |> List.filter_map ~f:parse_line
;;

module Branch = struct
  type t =
    { head : string
    ; ahead : int
    ; behind : int
    }
end

(* The --branch headers: "# branch.head main" and, with an upstream,
   "# branch.ab +2 -1". *)
let branch output =
  let find prefix =
    List.find_map (String.split_lines output) ~f:(fun l -> String.chop_prefix l ~prefix)
  in
  match find "# branch.head " with
  | None -> None
  | Some head ->
    let ahead, behind =
      match Option.map (find "# branch.ab ") ~f:(String.split ~on:' ') with
      | Some [ a; b ] ->
        ( Option.value (Int.of_string_opt a) ~default:0
        , -Option.value (Int.of_string_opt b) ~default:0 )
      | Some _ | None -> 0, 0
    in
    Some { Branch.head; ahead; behind }
;;

(* ---- name-status (the committed pane's entry source) ------------------- *)

(* [git diff --name-status] lines: "M<TAB>path", "R100<TAB>old<TAB>new",
   etc. Mapped into [Entry.t] with the letter in the INDEX slot (committed
   changes are "in the index's past"), worktree '.'; renames/copies keep
   the new path and carry [from]. Total: garbage lines are dropped. *)
let parse_name_status output =
  String.split_lines output
  |> List.filter_map ~f:(fun line ->
    match String.split line ~on:'\t' with
    | [ code; path ] when not (String.is_empty code) ->
      Some
        { Entry.index = code.[0]
        ; worktree = '.'
        ; path = unquote path
        ; kind = Entry.Kind.Changed
        }
    | [ code; from; path ] when not (String.is_empty code) ->
      Some
        { Entry.index = code.[0]
        ; worktree = '.'
        ; path = unquote path
        ; kind = Entry.Kind.Renamed { from = unquote from }
        }
    | _ -> None)
;;
