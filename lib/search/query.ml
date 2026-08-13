open! Core

module Term = struct
  type t =
    { text : string
    ; negated : bool
    ; fold_case : bool
    }
  [@@deriving sexp_of, compare, equal]
end

type t = Term.t list [@@deriving sexp_of, compare, equal]

let parse query =
  String.split query ~on:' '
  |> List.filter_map ~f:(fun word ->
    if String.is_empty word
    then None
    else (
      let negated = Char.equal word.[0] '!' in
      let text = if negated then String.drop_prefix word 1 else word in
      (* A lone [!] is the user mid-typing, not a term. *)
      if String.is_empty text
      then None
      else (
        let fold_case = not (String.exists text ~f:Char.is_uppercase) in
        Some { Term.text; negated; fold_case })))
;;

let is_empty = List.is_empty

(* The palette's greedy scan, keeping the byte positions it matched at.
   Greedy is fine for a boolean subsequence test: taking the earliest
   possible occurrence of each character never forecloses a later one. *)
let scan ({ text; fold_case; negated = _ } : Term.t) candidate =
  let tlen = String.length text in
  let positions = ref [] in
  let ti = ref 0 in
  String.iteri candidate ~f:(fun i c ->
    if !ti < tlen
    then (
      let q = text.[!ti] in
      let hit =
        if fold_case then Char.equal (Char.lowercase c) (Char.lowercase q) else Char.equal c q
      in
      if hit
      then (
        positions := i :: !positions;
        incr ti)));
  if !ti = tlen then Some (List.rev !positions) else None
;;

(* The boolean-only scan, allocation-free and exiting the moment the
   term is exhausted: it is the innermost loop of the full-file diff
   search and the per-keystroke tree narrowing, where [scan]'s dead
   positions list is pure GC churn. *)
let holds ({ text; fold_case; negated = _ } : Term.t) candidate =
  let tlen = String.length text
  and clen = String.length candidate in
  let ti = ref 0
  and ci = ref 0 in
  while !ti < tlen && !ci < clen do
    let c = candidate.[!ci]
    and q = text.[!ti] in
    let hit =
      if fold_case then Char.equal (Char.lowercase c) (Char.lowercase q) else Char.equal c q
    in
    if hit then incr ti;
    incr ci
  done;
  !ti = tlen
;;

let term_holds term candidate = Bool.( <> ) (holds term candidate) term.Term.negated
let matches t candidate = List.for_all t ~f:(fun term -> term_holds term candidate)

let positions t candidate =
  if not (matches t candidate)
  then None
  else
    Some
      (List.concat_map t ~f:(fun term ->
         if term.Term.negated then [] else Option.value (scan term candidate) ~default:[])
       |> List.dedup_and_sort ~compare:Int.compare)
;;

let spans t candidate =
  Option.map (positions t candidate) ~f:(fun positions ->
    List.fold positions ~init:[] ~f:(fun acc p ->
      match acc with
      | (start, stop) :: rest when p = stop -> (start, stop + 1) :: rest
      | acc -> (p, p + 1) :: acc)
    |> List.rev)
;;

let runs ~spans ~offset label =
  let len = String.length label in
  let spans =
    List.filter_map spans ~f:(fun (start, stop) ->
      let start = Int.max 0 (start - offset)
      and stop = Int.min len (stop - offset) in
      Option.some_if (stop > start) (start, stop))
  in
  let rec go at spans acc =
    match spans with
    | [] ->
      if at < len then (String.sub label ~pos:at ~len:(len - at), false) :: acc else acc
    | (start, stop) :: rest ->
      let acc =
        if start > at
        then (String.sub label ~pos:at ~len:(start - at), false) :: acc
        else acc
      in
      go stop rest ((String.sub label ~pos:start ~len:(stop - start), true) :: acc)
  in
  List.rev (go 0 spans [])
;;
