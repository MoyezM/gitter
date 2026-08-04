open! Core

(* The pure heart of Layout: a split tree plus a rectangle in, concrete
   rectangles out. No bonsai, no rendering — just geometry, so it's unit
   testable. Rendering, divider hit-testing, and mouse routing all read the
   same [Solved.t], which is what makes "what you see" and "what you click"
   incapable of disagreeing.

   The core tree is BINARY: every split has exactly two children and one
   divider, so sizing a split is a single clamped boundary and resizing is
   setting one fraction. N-ary layouts are surface syntax — [Tree.node]
   desugars a weighted child list into nested binary splits with the same
   proportions. *)

module Tree = struct
  type t =
    | Leaf of string
    | Split of
        { axis : Geometry.Axis.t
        ; frac : float (* [first]'s share of the axis, before clamping *)
        ; first : t
        ; second : t
        }

  (* N-ary sugar: fold weighted children into nested binary splits.
     [node `Row [ w1, a; w2, b; w3, c ]] gives [a] a w1/(w1+w2+w3) share at
     the outer split, then [b] and [c] divide the rest — exactly the n-ary
     proportions. Deterministic, so per-split identity (paths) is stable. *)
  let rec node axis children =
    match children with
    | [] -> failwith "Layout.Tree.node: a split needs at least one child"
    | [ (_, only) ] -> only
    | (weight, first) :: rest ->
      let total = weight +. List.sum (module Float) rest ~f:fst in
      let frac = if Float.( <= ) total 0. then 0.5 else weight /. total in
      Split { axis; frac; first; second = node axis rest }
  ;;
end

(* Every leaf is drawn inside its own border box, so its minimum outer size
   is 3x3 (borders + one cell of content). *)
let min_leaf = 3

(* A subtree's minimum extent along [axis]: same-axis splits need the sum of
   their children's minimums; cross-axis splits and leaves need one pane. *)
let rec min_along axis (tree : Tree.t) =
  match tree with
  | Leaf _ -> min_leaf
  | Split { axis = split_axis; first; second; _ } ->
    if Geometry.Axis.equal split_axis axis
    then min_along axis first + min_along axis second
    else Int.max (min_along axis first) (min_along axis second)
;;

let path_key path = String.concat ~sep:"/" (List.map (List.rev path) ~f:Int.to_string)

module Solved = struct
  type leaf =
    { id : string
    ; rect : Geometry.Rect.t
    }
  [@@deriving sexp]

  (* The 2-cell strip where the two children's borders meet. [split_rect] is
     the whole split's rect, so a drag can be read as a fraction of it. *)
  type divider =
    { path : string (* the split node, as a path key *)
    ; axis : Geometry.Axis.t
    ; rect : Geometry.Rect.t
    ; split_rect : Geometry.Rect.t
    }
  [@@deriving sexp]

  type t =
    { leaves : leaf list
    ; dividers : divider list
    }
  [@@deriving sexp]
end

let empty = { Solved.leaves = []; dividers = [] }

let merge (a : Solved.t) (b : Solved.t) =
  { Solved.leaves = a.leaves @ b.leaves; dividers = a.dividers @ b.dividers }
;;

(* [first]'s size along the axis: its fraction of the total, clamped so both
   sides keep their minimums, then capped to the budget when even the
   minimums don't fit. *)
let first_size ~total ~frac ~min_first ~min_second =
  let frac = Float.min 1. (Float.max 0. frac) in
  Float.of_int total *. frac
  |> Float.round_nearest
  |> Int.of_float
  |> Int.min (total - min_second)
  |> Int.max min_first
  |> Int.min total
;;

let carve ~axis ~(rect : Geometry.Rect.t) ~size_first =
  match (axis : Geometry.Axis.t) with
  | `Row ->
    ( { rect with width = size_first }
    , { rect with x = rect.x + size_first; width = rect.width - size_first } )
  | `Col ->
    ( { rect with height = size_first }
    , { rect with y = rect.y + size_first; height = rect.height - size_first } )
;;

(* The 2-cell strip straddling [first]'s trailing border and [second]'s
   leading border. *)
let divider_after ~axis ~(rect : Geometry.Rect.t) (first : Geometry.Rect.t) =
  match (axis : Geometry.Axis.t) with
  | `Row -> { rect with x = first.x + first.width - 1; width = 2 }
  | `Col -> { rect with y = first.y + first.height - 1; height = 2 }
;;

let rec solve_tree tree ~fractions ~path ~(rect : Geometry.Rect.t) =
  if rect.width <= 0 || rect.height <= 0
  then empty
  else (
    match (tree : Tree.t) with
    | Leaf id -> { empty with leaves = [ { Solved.id; rect } ] }
    | Split { axis; frac; first; second } ->
      let key = path_key path in
      let frac = Map.find fractions key |> Option.value ~default:frac in
      let total = Geometry.Axis.size axis rect in
      let size_first =
        first_size
          ~total
          ~frac
          ~min_first:(min_along axis first)
          ~min_second:(min_along axis second)
      in
      let first_rect, second_rect = carve ~axis ~rect ~size_first in
      let divider =
        (* No divider when one side is degenerate (extreme squeeze). *)
        if size_first > 0 && size_first < total
        then
          [ { Solved.path = key
            ; axis
            ; rect = divider_after ~axis ~rect first_rect
            ; split_rect = rect
            }
          ]
        else []
      in
      merge
        (merge
           { empty with dividers = divider }
           (solve_tree first ~fractions ~path:(0 :: path) ~rect:first_rect))
        (solve_tree second ~fractions ~path:(1 :: path) ~rect:second_rect))
;;

let solve tree ~fractions ~rect = solve_tree tree ~fractions ~path:[] ~rect

(* Dividers take precedence over leaves at the caller (a divider strip
   overlaps the adjacent leaves' border cells). *)
let divider_at (t : Solved.t) ~x ~y =
  List.find t.dividers ~f:(fun d -> Geometry.Rect.contains d.rect ~x ~y)
;;

let leaf_at (t : Solved.t) ~x ~y =
  List.find t.leaves ~f:(fun l -> Geometry.Rect.contains l.rect ~x ~y)
;;
