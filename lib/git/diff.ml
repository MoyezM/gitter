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
    ; old_count : int (* lines the hunk spans on the pre-image side *)
    ; new_start : int (* first line number on the post-image side *)
    ; new_count : int
    ; lines : Line.t list
    ; raw : string
      (* the hunk verbatim (header + body incl. "\ No newline" markers) —
         reconstructing from [lines] would corrupt patches, so staging
         builds its [git apply --cached] input from this *)
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

  (* The line just past the hunk, and the line just before it — the first
     and last line of the runs git elided on either side.

     A count of ZERO is why these are functions rather than arithmetic at
     the call site: git writes the line the hunk is anchored AFTER, so
     "-4,0" means "between old lines 4 and 5", not "an empty span at 4".
     Deriving the extent from [lines] instead gets that case backwards,
     and [diff.context = 0] — honored, since no query passes -U — makes
     EVERY hunk that case. *)
  let after start count = if count > 0 then start + count else start + 1
  let before start count = if count > 0 then start - 1 else start
  let old_after t = after t.old_start t.old_count
  let old_before t = before t.old_start t.old_count
  let new_after t = after t.new_start t.new_count
  let new_before t = before t.new_start t.new_count

  (* Does the body deliver what the header promised? The header counts and
     the parsed body are derived independently, so agreement is real
     evidence that neither the @@ line nor [body_line] lost anything —
     which is what the context-expansion gaps are measured against. Fails
     closed: a disagreement disables expansion for the file rather than
     shifting line numbers under it. *)
  let counts_agree t =
    let old_n, new_n =
      List.fold t.lines ~init:(0, 0) ~f:(fun (o, n) line ->
        match line with
        | Line.Context _ -> o + 1, n + 1
        | Removed _ -> o + 1, n
        | Added _ -> o, n + 1)
    in
    old_n = t.old_count && new_n = t.new_count
  ;;
end

module File = struct
  type t =
    { path : string (* the post-image path *)
    ; hunks : Hunk.t list
    }
end

(* "diff --git a/foo b/foo" -> "foo". Anchored on the LAST " b/" rather
   than the last space: git does not quote spaces in this header, so
   splitting on whitespace turns "a/my file.txt b/my file.txt" into
   "file.txt" — a path that matches nothing. The last occurrence is the
   right one for renames, whose a-side may itself contain " b/". *)
let path_of_header line =
  match String.substr_index_all line ~may_overlap:false ~pattern:" b/" |> List.last with
  | Some i -> String.drop_prefix line (i + 3)
  | None ->
    (match String.rsplit2 line ~on:' ' with
     | Some (_, b_path) -> String.chop_prefix_if_exists b_path ~prefix:"b/"
     | None -> line)
;;

(* "@@ -13,22 +13,17 @@ ..." -> Some ((13, 22), (13, 17)). A count is
   omitted when it is 1 ("@@ -13 +13 @@").

   [None] rather than a guess, because (0, 0) for garbage is
   indistinguishable from a legitimate "-0,0" new-file hunk — and the two
   want opposite treatment: one must be dropped, the other is a real hunk
   anchored before line 1. *)
let hunk_spans header =
  let span token ~sign =
    match String.chop_prefix token ~prefix:sign with
    | None -> None
    | Some rest ->
      let start, count =
        match String.lsplit2 rest ~on:',' with
        | Some (start, count) -> start, count
        | None -> rest, "1"
      in
      (match Int.of_string_opt start, Int.of_string_opt count with
       | Some start, Some count when start >= 0 && count >= 0 -> Some (start, count)
       | _ -> None)
  in
  match String.split header ~on:' ' with
  | at :: old_tok :: new_tok :: _ when String.equal at "@@" ->
    Option.both (span old_tok ~sign:"-") (span new_tok ~sign:"+")
  | _ -> None
;;

(* A hunk-body line; None drops "\ No newline at end of file" and junk.

   The zero-length arm is not defensive: with [diff.suppressBlankEmpty]
   git writes a blank context line as an EMPTY line rather than a lone
   space, and dropping it silently shifts every subsequent line number in
   the hunk. "\ No newline" starts with a backslash and [String.split_lines]
   never manufactures a trailing "", so in unified-diff output a
   zero-length body line can only be a suppressed blank context line. *)
let body_line line =
  match String.prefix line 1 with
  | "+" -> Some (Line.Added (String.drop_prefix line 1))
  | "-" -> Some (Line.Removed (String.drop_prefix line 1))
  | " " -> Some (Line.Context (String.drop_prefix line 1))
  | "" -> Some (Line.Context "")
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

(* [None] when the @@ line does not parse: a hunk anchored at a guessed
   line 0 is worse than an absent one — staging would apply [raw] to the
   wrong place, and the gap arithmetic would measure from nowhere. *)
let hunk_of_run (header, body) =
  Option.map (hunk_spans header) ~f:(fun ((old_start, old_count), (new_start, new_count)) ->
    { Hunk.header
    ; old_start
    ; old_count
    ; new_start
    ; new_count
    ; lines = List.filter_map body ~f:body_line
    ; raw = String.concat ~sep:"\n" (header :: body) ^ "\n"
    })
;;

let file_of_run (header, rest) =
  let ~leading:_preamble, ~runs = split_runs ~marker:"@@" rest in
  { File.path = path_of_header header; hunks = List.filter_map runs ~f:hunk_of_run }
;;

let parse output : File.t list =
  let ~leading:_junk, ~runs = split_runs ~marker:"diff --git" (String.split_lines output) in
  List.map runs ~f:file_of_run
;;

(* ---- numstat ----------------------------------------------------------- *)

(* The numstat path field spells renames "old => new", with a brace form
   when the sides share affixes: "pre{old => new}post". Resolve to the NEW
   path — the one status reports and the panes key on. *)
let numstat_path field =
  match String.substr_index field ~pattern:" => " with
  | None -> field
  | Some arrow ->
    (match String.lsplit2 field ~on:'{' with
     | Some (pre, rest) ->
       (match String.lsplit2 rest ~on:'}' with
        | Some (inside, post) ->
          let after =
            match String.substr_index inside ~pattern:" => " with
            | Some i -> String.drop_prefix inside (i + 4)
            | None -> inside
          in
          pre ^ after ^ post
        | None -> String.drop_prefix field (arrow + 4))
     | None -> String.drop_prefix field (arrow + 4))
;;

(* "added<TAB>removed<TAB>path" lines; binary files print "-" and are
   dropped. Total: garbage lines are skipped, never raised. *)
let numstat output =
  String.split_lines output
  |> List.filter_map ~f:(fun line ->
    match String.split line ~on:'\t' with
    | [ added; removed; path ] ->
      (match Int.of_string_opt added, Int.of_string_opt removed with
       | Some a, Some d -> Some (numstat_path path, (a, d))
       | _ -> None)
    | _ -> None)
;;
