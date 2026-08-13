open! Core

type t = { mutable memo : (Query.t * int array) option }

let create () = { memo = None }

(* Whether [query] can only SHRINK [old_query]'s match set — typing's
   common shapes: appending characters to the last positive term, or
   appending whole new terms (each term only constrains further). Not
   refinable: extending a NEGATED term (its exclusions loosen, so
   matches can grow) or flipping a term's smart-case. *)
let refines ~old_query ~query =
  let rec go (olds : Query.Term.t list) (news : Query.Term.t list) =
    match olds, news with
    | [], _ -> true
    | _ :: _, [] -> false
    | [ o ], n :: _ ->
      Query.Term.equal o n
      || ((not o.negated)
          && (not n.negated)
          && Bool.equal o.fold_case n.fold_case
          && String.is_prefix n.text ~prefix:o.text)
    | o :: olds, n :: news -> Query.Term.equal o n && go olds news
  in
  go old_query query
;;

let find t ~query ~length ~candidate =
  if Query.is_empty query
  then [||]
  else (
    match t.memo with
    | Some (q, positions) when Query.equal q query -> positions
    | memo ->
      let matches_at i =
        match candidate i with
        | Some text -> Query.matches query text
        | None -> false
      in
      let positions =
        match memo with
        | Some (old_query, old_positions) when refines ~old_query ~query ->
          Array.filter old_positions ~f:matches_at
        | _ ->
          let out = Queue.create () in
          for i = 0 to length - 1 do
            if matches_at i then Queue.enqueue out i
          done;
          Queue.to_array out
      in
      t.memo <- Some (query, positions);
      positions)
;;

let next ?(inclusive = false) ~dir ~current candidates =
  let n = Array.length candidates in
  if n = 0
  then None
  else (
    let which =
      if dir >= 0
      then if inclusive then `First_greater_than_or_equal_to else `First_strictly_greater_than
      else if inclusive
      then `Last_less_than_or_equal_to
      else `Last_strictly_less_than
    in
    let index =
      match Array.binary_search candidates which current ~compare:Int.compare with
      | Some i -> i
      | None -> if dir >= 0 then 0 else n - 1 (* wrap *)
    in
    Some candidates.(index))
;;
