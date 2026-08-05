open! Core
open Gitter.Layout.Solver
module Rect = Gitter.Layout.Geometry.Rect

let check name cond = if not cond then failwithf "FAILED: %s" name ()
let rect ~x ~y ~width ~height = { Rect.x; y; width; height }
let full = rect ~x:0 ~y:0 ~width:80 ~height:24
let no_overrides = String.Map.empty

let leaf_rect solved id =
  List.find_exn solved.Solved.leaves ~f:(fun l -> String.equal l.id id)
  |> fun (l : Solved.leaf) -> l.rect
;;

(* Leaves of a solve must tile their axis exactly: no gaps, no overlap. *)
let () =
  let tree = Tree.node `Row [ 1., Tree.Leaf "a"; 1., Leaf "b" ] in
  let solved = solve tree ~fractions:no_overrides ~rect:full in
  let a = leaf_rect solved "a" in
  let b = leaf_rect solved "b" in
  check "equal row split: a starts at 0" (a.x = 0);
  check "equal row split: halves" (a.width = 40 && b.width = 40);
  check "equal row split: adjacent" (b.x = a.x + a.width);
  check "equal row split: full height" (a.height = 24 && b.height = 24)
;;

(* Weights are respected through the n-ary -> binary desugaring. *)
let () =
  let tree = Tree.node `Row [ 1., Tree.Leaf "a"; 3., Leaf "b" ] in
  let solved = solve tree ~fractions:no_overrides ~rect:full in
  check "1:3 split" ((leaf_rect solved "a").width = 20 && (leaf_rect solved "b").width = 60)
;;

(* Nested splits partition recursively; sizes always sum to the parent. *)
let () =
  let tree =
    Tree.node
      `Row
      [ 1., Tree.Leaf "left"
      ; 2., Tree.node `Col [ 2., Tree.Leaf "top"; 1., Leaf "bottom" ]
      ]
  in
  let solved = solve tree ~fractions:no_overrides ~rect:full in
  let left = leaf_rect solved "left" in
  let top = leaf_rect solved "top" in
  let bottom = leaf_rect solved "bottom" in
  check "nested: widths partition" (left.width + top.width = 80);
  check "nested: right column shares x" (top.x = bottom.x && top.x = left.width);
  check "nested: heights partition" (top.height + bottom.height = 24);
  check "nested: vertical adjacency" (bottom.y = top.y + top.height)
;;

(* Fraction overrides replace declared fractions, keyed by the split's
   leftmost leaves so they survive pane hiding; unknown keys are inert. *)
let () =
  let tree = Tree.node `Row [ 1., Tree.Leaf "a"; 1., Leaf "b" ] in
  let overrides = String.Map.of_alist_exn [ "a|b", 0.75 ] in
  let solved = solve tree ~fractions:overrides ~rect:full in
  check "override applied" ((leaf_rect solved "a").width = 60);
  let stale = String.Map.of_alist_exn [ "ghost|ghost", 0.9 ] in
  let solved = solve tree ~fractions:stale ~rect:full in
  check "unknown-key override ignored" ((leaf_rect solved "a").width = 40);
  (* The point of content keys: the boundary between b and c keeps its
     dragged fraction whether or not a is in the tree. *)
  let three = Tree.node `Row [ 1., Tree.Leaf "a"; 1., Leaf "b"; 1., Leaf "c" ] in
  let two = Tree.node `Row [ 1., Tree.Leaf "b"; 1., Leaf "c" ] in
  let bc = String.Map.of_alist_exn [ "b|c", 0.9 ] in
  let with_a = solve three ~fractions:bc ~rect:full in
  let without_a = solve two ~fractions:bc ~rect:full in
  check "b|c override applies in the full tree"
    ((leaf_rect with_a "c").width < 20);
  check "b|c override survives hiding a"
    ((leaf_rect without_a "b").width = 72)
;;

(* Minimums hold: an extreme weight cannot crush a sibling below min. *)
let () =
  let tree = Tree.node `Row [ 1000., Tree.Leaf "a"; 1., Leaf "b" ] in
  let solved = solve tree ~fractions:no_overrides ~rect:full in
  check "min respected" ((leaf_rect solved "b").width >= min_leaf);
  check "still sums" ((leaf_rect solved "a").width + (leaf_rect solved "b").width = 80)
;;

(* The binary-specific hazard: a same-axis CHAIN's minimum is the sum of its
   leaves' minimums, so a huge head weight cannot crush the tail chain. *)
let () =
  let tree =
    Tree.node `Row [ 1000., Tree.Leaf "a"; 1., Tree.Leaf "b"; 1., Leaf "c" ]
  in
  let solved = solve tree ~fractions:no_overrides ~rect:full in
  check "chain min: b" ((leaf_rect solved "b").width >= min_leaf);
  check "chain min: c" ((leaf_rect solved "c").width >= min_leaf);
  check
    "chain still sums"
    ((leaf_rect solved "a").width
     + (leaf_rect solved "b").width
     + (leaf_rect solved "c").width
     = 80)
;;

(* Impossible fits degrade without crashing and never exceed the budget. *)
let () =
  let tree =
    Tree.node `Row (List.init 6 ~f:(fun i -> 1., Tree.Leaf (sprintf "l%d" i)))
  in
  let tiny = rect ~x:0 ~y:0 ~width:10 ~height:5 in
  let solved = solve tree ~fractions:no_overrides ~rect:tiny in
  let total = List.sum (module Int) solved.Solved.leaves ~f:(fun l -> l.rect.width) in
  check "tiny: never exceeds budget" (total <= 10)
;;

(* Dividers: one per binary split, sitting on the shared border. *)
let () =
  let tree = Tree.node `Row [ 1., Tree.Leaf "a"; 1., Leaf "b" ] in
  let solved = solve tree ~fractions:no_overrides ~rect:full in
  check "one divider" (List.length solved.Solved.dividers = 1);
  let d = List.hd_exn solved.Solved.dividers in
  check "divider position" (d.rect.x = 39 && d.rect.width = 2);
  check "divider_at hits" (Option.is_some (divider_at solved ~x:40 ~y:5));
  check "divider_at misses" (Option.is_none (divider_at solved ~x:10 ~y:5));
  check
    "leaf_at finds b"
    (match leaf_at solved ~x:60 ~y:5 with
     | Some l -> String.equal l.id "b"
     | None -> false);
  (* Three children desugar to two binary splits: two dividers. *)
  let three = Tree.node `Row [ 1., Tree.Leaf "a"; 1., Tree.Leaf "b"; 1., Leaf "c" ] in
  let solved = solve three ~fractions:no_overrides ~rect:full in
  check "two dividers for three panes" (List.length solved.Solved.dividers = 2)
;;

(* Zoom is just solving a single leaf: full rect, no dividers. *)
let () =
  let solved = solve (Tree.Leaf "only") ~fractions:no_overrides ~rect:full in
  check "zoom: full rect" (Rect.equal (leaf_rect solved "only") full);
  check "zoom: no dividers" (List.is_empty solved.Solved.dividers)
;;

let () = print_endline "All solver tests passed."
