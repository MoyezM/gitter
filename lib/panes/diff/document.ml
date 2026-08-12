open! Core

type line =
  | File_header of string
  | Rule of
      { hidden : int
      ; old_no : int
      ; new_no : int
      ; label : string
      }
  | Diff_line of int option * int option * Git.Diff.Line.t

type row =
  { line : line
  ; revealed : bool
  ; pos : int
  }

type t = row array

let is_cursor_row = function
  | Diff_line _ -> true
  | Rule { hidden; _ } -> hidden > 0
  | File_header _ -> false
;;

(* Greatest i with [t.(i).pos <= pos]; rows ascend by [pos], so a hidden
   position lands on the rule covering it. *)
let index_of_pos (t : t) pos =
  match
    Array.binary_search t `Last_less_than_or_equal_to pos ~compare:(fun r p ->
      Int.compare r.pos p)
  with
  | Some i -> i
  | None -> 0
;;

module Source = struct
  type t =
    { hunks : Git.Diff.Hunk.t array
      (* the single file's hunks, ordered — [||] for multi-file sources,
         which never stage; ownership queries binary-search this *)
    ; full : row array (* the whole file; [pos] = index *)
    ; up : int array (* per row: distance to the nearest change ABOVE it *)
    ; down : int array (* and BELOW — [min up down] is plain distance *)
    ; runkey : int array
      (* per row, the elided RUN it belongs to — keyed by the run's first
         pre-image line, a content key that survives refetches for
         untouched gaps and silently stops matching for staged-away ones;
         -1 on rows git shipped. Runs are static: the full file never
         changes, so a run cannot move, split or renumber. *)
    ; runs : (int * int * int * int) Int.Map.t
      (* per run: the (top, bottom) levels past which each end is fully
         open — top governed by [up], bottom by [down] — and the run's
         (first, last) positions, for pinning a directional reveal *)
    ; base : int (* the deepest context git itself shipped *)
    ; mutable memo : ((int * int) Int.Map.t * row array) option
    }

  let empty =
    { hunks = [||]
    ; full = [||]
    ; up = [||]
    ; down = [||]
    ; runkey = [||]
    ; runs = Int.Map.empty
    ; base = 0
    ; memo = None
    }
  ;;

  (* A source with nothing elided — what the fixtures and the fallback
     path share, so they cannot drift apart. *)
  let inert full =
    { empty with
      full
    ; up = Array.create ~len:(Array.length full) 0
    ; down = Array.create ~len:(Array.length full) 0
    ; runkey = Array.create ~len:(Array.length full) (-1)
    }
  ;;

  let of_rows rows = inert (Array.of_list rows |> Array.mapi ~f:(fun pos r -> { r with pos }))

  (* The plain flattened diff, as the pane has always shown it: a labeled
     rule per hunk, then its rows. The fallback when interleaving cannot
     be trusted — nothing is hidden, so the mask never widens. *)
  let plain files =
    let many = List.length files > 1 in
    let out = Queue.create () in
    List.iter files ~f:(fun (file : Git.Diff.File.t) ->
      if many then Queue.enqueue out (File_header file.path);
      List.iter file.hunks ~f:(fun (h : Git.Diff.Hunk.t) ->
        Queue.enqueue
          out
          (Rule { hidden = 0; old_no = h.old_start; new_no = h.new_start; label = h.header });
        List.iter (Git.Diff.Hunk.numbered h) ~f:(fun (o, n, l) ->
          Queue.enqueue out (Diff_line (o, n, l)))));
    Queue.to_array out
  ;;

  (* Every line of the file, in order: the hunks' own rows verbatim, and
     the blob's lines threaded between them, each tagged with whether git
     shipped it. [None] when the diff and the blob cannot describe the
     same file: @@ counts that disagree with the body, a gap that measures
     differently on the two sides, a sampled line the blob contradicts (a
     symlink target, LFS pointer or clean filter), or lengths that
     disagree past the last hunk. *)
  let interleave (file : Git.Diff.File.t) ~blob ~new_len =
    let hunks = Array.of_list file.hunks in
    let n = Array.length hunks in
    let ok = ref (n > 0 && Array.for_all hunks ~f:Git.Diff.Hunk.counts_agree) in
    (match
       Array.find_map hunks ~f:(fun h ->
         List.find_map (Git.Diff.Hunk.numbered h) ~f:(fun (o, _, l) ->
           match o, l with
           | Some o, (Git.Diff.Line.Context s | Removed s) -> Some (o, s)
           | _ -> None))
     with
     | Some (o, s) ->
       if not (o >= 1 && o <= Array.length blob && String.equal blob.(o - 1) s)
       then ok := false
     | None -> ok := false);
    if n > 0
    then (
      let last = hunks.(n - 1) in
      if Array.length blob - Git.Diff.Hunk.old_after last
         <> new_len - Git.Diff.Hunk.new_after last
      then ok := false);
    let out = Queue.create () in
    for k = 0 to n do
      let old_lo = if k = 0 then 1 else Git.Diff.Hunk.old_after hunks.(k - 1)
      and new_lo = if k = 0 then 1 else Git.Diff.Hunk.new_after hunks.(k - 1) in
      let old_hi = if k = n then Array.length blob else Git.Diff.Hunk.old_before hunks.(k) in
      if old_hi > Array.length blob then ok := false;
      if k < n && new_lo + (old_hi - old_lo) <> Git.Diff.Hunk.new_before hunks.(k)
      then ok := false;
      for o = old_lo to Int.min old_hi (Array.length blob) do
        Queue.enqueue
          out
          (Diff_line (Some o, Some (new_lo + o - old_lo), Git.Diff.Line.Context blob.(o - 1)), false)
      done;
      if k < n
      then
        List.iter (Git.Diff.Hunk.numbered hunks.(k)) ~f:(fun (o, nn, l) ->
          Queue.enqueue out (Diff_line (o, nn, l), true))
    done;
    if !ok then Some (Queue.to_array out) else None
  ;;

  let create files ~old_text ~new_text =
    let blob = Array.of_list (String.split_lines old_text) in
    let new_len = List.length (String.split_lines new_text) in
    (* Single-file only, decided ONCE: the blob can be authoritative for
       one file, and only single-file sides stage. [hunks] stays populated
       even when interleaving fails, so the plain fallback still stages. *)
    let cells, hunks =
      match files with
      | [ (file : Git.Diff.File.t) ] ->
        interleave file ~blob ~new_len, Array.of_list file.hunks
      | [] | _ :: _ :: _ -> None, [||]
    in
    match cells with
    | None ->
      let full =
        Array.mapi (plain files) ~f:(fun pos line -> { line; revealed = false; pos })
      in
      { (inert full) with hunks }
    | Some cells ->
      let n = Array.length cells in
      (* Distance to the nearest changed row, per DIRECTION: [up] scans
         forward carrying distance from the change above, [down] backward
         from the change below. Their min is plain distance; keeping both
         is what lets each end of a run open independently. *)
      let seed =
        Array.map cells ~f:(fun (line, _) ->
          match line with
          | Diff_line (_, _, (Added _ | Removed _)) -> 0
          | _ -> Int.max_value / 2)
      in
      let up = Array.copy seed
      and down = Array.copy seed in
      for i = 1 to n - 1 do
        up.(i) <- Int.min up.(i) (up.(i - 1) + 1)
      done;
      for i = n - 2 downto 0 do
        down.(i) <- Int.min down.(i) (down.(i + 1) + 1)
      done;
      let dist i = Int.min up.(i) down.(i) in
      let base = ref 0 in
      Array.iteri cells ~f:(fun i (_, shipped) ->
        if shipped then base := Int.max !base (dist i));
      (* The elided runs: maximal blocks of rows git did not ship, each
         keyed by its first pre-image line. *)
      let runkey = Array.create ~len:n (-1) in
      let runs = ref Int.Map.empty in
      let key = ref (-1) in
      Array.iteri cells ~f:(fun i (line, shipped) ->
        if shipped
        then key := -1
        else (
          (if !key < 0
           then
             match line with
             | Diff_line (Some o, _, _) -> key := o
             | _ -> ());
          if !key >= 0
          then (
            runkey.(i) <- !key;
            runs
            := Map.update !runs !key ~f:(fun m ->
                 let t, b, first, _ = Option.value m ~default:(0, 0, i, i) in
                 Int.max t up.(i), Int.max b down.(i), first, i))));
      let full =
        Array.mapi cells ~f:(fun pos (line, shipped) ->
          { line; revealed = not shipped; pos })
      in
      { hunks; full; up; down; runkey; runs = !runs; base = !base; memo = None }
  ;;
end

let base_context (s : Source.t) = s.base
let run_max (s : Source.t) key =
  match Map.find s.Source.runs key with
  | Some (t, b, _, _) -> t, b
  | None -> 0, 0
;;

let runs (s : Source.t) = Map.keys s.Source.runs

let key_of (s : Source.t) (doc : t) i =
  if i < 0 || i >= Array.length doc
  then None
  else (
    let p = doc.(i).pos in
    if p >= 0 && p < Array.length s.Source.runkey && s.Source.runkey.(p) >= 0
    then Some s.Source.runkey.(p)
    else None)
;;

let rec scan s doc i step =
  if i < 0 || i >= Array.length doc
  then None
  else (
    match key_of s doc i with
    | Some k -> Some k
    | None -> scan s doc (i + step) step)
;;

(* The run [K]/[J] act on: the row's own ([scan] starts at [row]), else
   the nearest one in that direction alone — "expand up" from inside a
   hunk reaches the boundary ABOVE it, never one below. *)
let run_toward (s : Source.t) (doc : t) ~row ~dir =
  if Map.is_empty s.Source.runs
  then None
  else scan s doc row (match dir with `Up -> -1 | `Down -> 1)
;;

(* The elided run at [row], preferring above — what [X] and a click act
   on. Defined by the directional rule so the two targeting paths cannot
   diverge. *)
let run_at s doc ~row =
  Option.first_some (run_toward s doc ~row ~dir:`Up) (run_toward s doc ~row ~dir:`Down)
;;

(* The run's extent in file positions — what a directional reveal PINS
   against: the row just past one end holds its screen line while the new
   rows push the rest of the view away from it. *)
let run_span (s : Source.t) key =
  Option.map (Map.find s.Source.runs key) ~f:(fun (_, _, first, last) -> first, last)
;;

(* The hunk a file line belongs to, from the original parse: containment,
   else the nearer of the two neighbours (ties up) — so a gap row goes
   with the change the reader is closest to. Hunks ascend in both
   coordinates, so this is a binary search plus one comparison; it runs
   per staging keypress AND per rule labeled during a mask build, where a
   linear walk would make the build O(hunks²). Single-file only: line
   numbers repeat across files, and only single-file sides stage. *)
let nearest_hunk (s : Source.t) ~old_no ~new_no =
  let hunks = s.Source.hunks in
  let start, stop =
    match old_no, new_no with
    | Some _, _ ->
      ( (fun (h : Git.Diff.Hunk.t) -> h.old_start)
      , fun h -> Int.max h.Git.Diff.Hunk.old_start (Git.Diff.Hunk.old_after h - 1) )
    | None, _ ->
      ( (fun (h : Git.Diff.Hunk.t) -> h.new_start)
      , fun h -> Int.max h.Git.Diff.Hunk.new_start (Git.Diff.Hunk.new_after h - 1) )
  in
  match Option.first_some old_no new_no with
  | None -> None
  | Some x ->
    (match
       Array.binary_search hunks `Last_less_than_or_equal_to x ~compare:(fun h x ->
         Int.compare (start h) x)
     with
     | None -> if Array.is_empty hunks then None else Some hunks.(0)
     | Some i ->
       let a = hunks.(i) in
       if x <= stop a || i + 1 >= Array.length hunks
       then Some a
       else (
         let b = hunks.(i + 1) in
         Some (if x - stop a <= start b - x then a else b)))
;;

(* The hunk a counted rule ANNOUNCES: the one whose first pre-image line
   is [old_no + hidden], just past the run — and none for the trailing
   run, which fronts nothing. Stated in parse coordinates so the mask
   loop needs no display-space special case for it. *)
let below_rule (s : Source.t) ~old_no ~hidden =
  let hunks = s.Source.hunks in
  let x = old_no + hidden in
  if Array.is_empty hunks || x > Git.Diff.Hunk.old_after hunks.(Array.length hunks - 1)
  then None
  else nearest_hunk s ~old_no:(Some x) ~new_no:None
;;

let hunk_under (s : Source.t) (doc : t) ~row =
  if row < 0 || row >= Array.length doc
  then None
  else (
    match doc.(row).line with
    | Diff_line (o, n, _) -> nearest_hunk s ~old_no:o ~new_no:n
    (* Staging on a rule is the NEAREST law, same as the run's own rows:
       an interior rule stages the hunk it announces, and the trailing
       rule the last hunk — what its rows stage once revealed. *)
    | Rule { hidden; old_no; _ } -> nearest_hunk s ~old_no:(Some (old_no + hidden)) ~new_no:None
    | File_header _ -> None)
;;

(* The mask: one inequality per row — its run's two thresholds, floored
   at what git shipped — with each maximal hidden run collapsed into a
   counted rule labeled for the hunk below it. *)
let mask (s : Source.t) ~levels : t =
  let { Source.full; up; down; runkey; base; _ } = s in
  let n = Array.length full in
  (* One [Map.find] per RUN, not per hidden row: [runkey] is constant
     across a run, and hidden rows are the overwhelming majority of a
     collapsed 646K-row file. *)
  let cached_key = ref Int.min_value
  and cached = ref None in
  let level_of key =
    if key <> !cached_key
    then (
      cached_key := key;
      cached := Map.find levels key);
    !cached
  in
  let visible i =
    Int.min up.(i) down.(i) <= base
    || (runkey.(i) >= 0
        &&
        match level_of runkey.(i) with
        | Some (t, b) -> up.(i) <= t || down.(i) <= b
        | None -> false)
  in
  let out = Queue.create () in
  let i = ref 0 in
  while !i < n do
    if visible !i
    then (
      Queue.enqueue out full.(!i);
      incr i)
    else (
      let start = !i in
      while !i < n && not (visible !i) do
        incr i
      done;
      let hidden = !i - start in
      let old_no, new_no =
        match full.(start).line with
        | Diff_line (o, nn, _) -> Option.value o ~default:1, Option.value nn ~default:1
        | _ -> 1, 1
      in
      let label =
        below_rule s ~old_no ~hidden
        |> Option.value_map ~default:"" ~f:(fun (h : Git.Diff.Hunk.t) -> h.header)
      in
      Queue.enqueue
        out
        { line = Rule { hidden; old_no; new_no; label }; revealed = false; pos = start })
  done;
  Queue.to_array out
;;

let of_source (s : Source.t) ~levels =
  match s.Source.memo with
  | Some (l, rows) when Map.equal [%equal: int * int] l levels -> rows
  | _ ->
    let rows = mask s ~levels in
    s.Source.memo <- Some (levels, rows);
    rows
;;
