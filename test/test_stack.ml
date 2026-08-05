open! Core
module Stack = Gitter.Git.Branch_stack

let check name cond = if not cond then failwithf "FAILED: %s" name ()

let names branches = List.map branches ~f:(fun (b : Stack.Branch.t) -> b.name)

let find branches name =
  List.find_exn branches ~f:(fun (b : Stack.Branch.t) -> String.equal b.name name)
;;

(* Shas are opaque strings to the parser — readable fixtures. *)

(* Linear stack main -> a -> b. *)
let () =
  let branches =
    Stack.parse
      ~heads:"main\tM\na\tA\nb\tB"
      ~dag:"B A\nA M"
      ~reflogs:""
      ~trunk:"main"
      ~current:(Some "b")
  in
  check "linear order: trunk first, tree grows down"
    (List.equal String.equal (names branches) [ "main"; "a"; "b" ]);
  check "depths follow the chain"
    (List.equal Int.equal (List.map branches ~f:(fun b -> b.depth)) [ 0; 1; 2 ]);
  check "current marked" (find branches "b").is_current;
  check "trunk marked" (find branches "main").is_trunk;
  check "no restack needed"
    (List.for_all branches ~f:(fun b -> not b.needs_restack))
;;

(* Amended parent: [b] sits on [a]'s OLD tip A1; a's head is now A2. Only
   the reflog re-associates them — and flags the restack. *)
let () =
  let branches =
    Stack.parse
      ~heads:"main\tM\na\tA2\nb\tB"
      ~dag:"B A1\nA1 M\nA2 M"
      ~reflogs:"a\tA2\na\tA1"
      ~trunk:"main"
      ~current:(Some "a")
  in
  check "amended parent keeps the chain"
    (List.equal String.equal (names branches) [ "main"; "a"; "b" ]);
  check "child of amended parent needs restack" (find branches "b").needs_restack;
  check "amended parent itself does not" (not (find branches "a").needs_restack);
  (* Without the reflog the association is genuinely invisible to git:
     both hang off the trunk. *)
  let no_reflog =
    Stack.parse
      ~heads:"main\tM\na\tA2\nb\tB"
      ~dag:"B A1\nA1 M\nA2 M"
      ~reflogs:""
      ~trunk:"main"
      ~current:None
  in
  check "no reflog: both attach to trunk"
    (List.for_all no_reflog ~f:(fun b -> b.depth <= 1))
;;

(* A fork: two independent branches off the trunk. *)
let () =
  let branches =
    Stack.parse
      ~heads:"main\tM\na\tA\nc\tC"
      ~dag:"A M\nC M"
      ~reflogs:""
      ~trunk:"main"
      ~current:None
  in
  check "fork shape (heads order = recency order)"
    (List.equal String.equal (names branches) [ "main"; "a"; "c" ]);
  let reordered =
    Stack.parse
      ~heads:"main\tM\nc\tC\na\tA"
      ~dag:"A M\nC M"
      ~reflogs:""
      ~trunk:"main"
      ~current:None
  in
  check "more recent sibling sorts first"
    (List.equal String.equal (names reordered) [ "main"; "c"; "a" ]);
  check "fork depths"
    (List.for_all branches ~f:(fun b -> b.depth = if b.is_trunk then 0 else 1))
;;

(* Fully merged branches disappear; a fresh branch AT the trunk head does
   not (same commit, distinct branch). *)
let () =
  let branches =
    Stack.parse
      ~heads:"main\tM2\nold\tO\nfresh\tM2"
      ~dag:"M2 M1\nM1 O\nO"
      ~reflogs:""
      ~trunk:"main"
      ~current:None
  in
  check "merged branch hidden, fresh branch kept"
    (List.equal String.equal (names branches) [ "main"; "fresh" ]);
  check "fresh branch is a trunk child" ((find branches "fresh").depth = 1);
  check "fresh branch needs no restack" (not (find branches "fresh").needs_restack)
;;

(* Trunk moved ahead of a stacked branch: needs restack. *)
let () =
  let branches =
    Stack.parse
      ~heads:"main\tM2\na\tA"
      ~dag:"M2 M1\nA M1\nM1"
      ~reflogs:""
      ~trunk:"main"
      ~current:(Some "a")
  in
  check "advanced trunk flags restack" (find branches "a").needs_restack
;;

(* Totality: no trunk head, empty inputs, garbage lines. *)
let () =
  check "unknown trunk yields empty"
    (List.is_empty (Stack.parse ~heads:"a\tA" ~dag:"" ~reflogs:"" ~trunk:"main" ~current:None));
  check "empty heads yield empty"
    (List.is_empty (Stack.parse ~heads:"" ~dag:"" ~reflogs:"" ~trunk:"main" ~current:None));
  check "garbage is dropped"
    (List.equal
       String.equal
       (names
          (Stack.parse
             ~heads:"main\tM\n\nnot a head line"
             ~dag:"\ngarbage-only"
             ~reflogs:"also\tgarbage\textra"
             ~trunk:"main"
             ~current:None))
       [ "main" ])
;;

(* The current branch's subtree leads its siblings even when another
   branch is more recent. *)
let () =
  let branches =
    Stack.parse
      ~heads:"main\tM\naaa\tA\nzzz\tZ"
      ~dag:"A M\nZ M"
      ~reflogs:""
      ~trunk:"main"
      ~current:(Some "zzz")
  in
  check "current stack leads its siblings"
    (List.equal String.equal (names branches) [ "main"; "zzz"; "aaa" ])
;;

(* Many fresh branches on the trunk head are trunk CHILDREN, not an
   alphabetical chain (the equal-head tiebreak only applies above the
   base). *)
let () =
  let branches =
    Stack.parse
      ~heads:"main\tM\np\tM\nq\tM\nr\tM"
      ~dag:""
      ~reflogs:""
      ~trunk:"main"
      ~current:None
  in
  check "at-base pile stays flat"
    (List.for_all branches ~f:(fun b -> b.depth = if b.is_trunk then 0 else 1))
;;

(* ---- the pane's fold/viewport state over the parsed stack -------------- *)

module PS = Gitter.Panes.Stack.State

let mk ?(current = false) ?(trunk = false) ?(restack = false) name depth =
  { Stack.Branch.name; depth; is_current = current; is_trunk = trunk; needs_restack = restack }
;;

(* DFS (= display) order: main(0) -> b1(1) -> b2(2, current) -> b3(3);
   plus off-chain leaves old, codex/one, codex/two, lone. *)
let branches =
  (
    [ mk "main" 0 ~trunk:true
    ; mk "b1" 1
    ; mk "b2" 2 ~current:true
    ; mk "b3" 3 ~restack:true
    ; mk "old" 1 ~restack:true
    ; mk "codex/one" 1
    ; mk "codex/two" 1
    ; mk "lone" 1
    ]
  : Stack.Branch.t list)
;;

module Listing = Gitter.Panes.Listing

let keys rows = List.map rows ~f:(fun (r : PS.Row.t) -> r.key)
let no_overrides = String.Map.empty

let psel key =
  { PS.Model.initial with
    listing = { Listing.Model.initial with selection = Some key }
  }
;;

let () =
  let rows = PS.visible ~branches ~overrides:no_overrides in
  check
    "defaults: chain expanded, slash group + leaves as single rows"
    (List.equal
       String.equal
       (keys rows)
       [ "main"; "b1"; "b2"; "b3"; "old"; "main//codex"; "lone" ]);
  let group = List.find_exn rows ~f:(fun r -> String.equal r.key "main//codex") in
  check "group is collapsed with its members counted"
    (group.collapsed && group.hidden = 2 && group.has_children);
  check "group renders as a Group row"
    (match group.PS.Row.kind with PS.Row.Group { prefix } -> String.equal prefix "codex" | _ -> false);
  check "chain rows are on the current stack"
    (List.for_all rows ~f:(fun r ->
       let on = List.mem [ "b3"; "b2"; "b1"; "main" ] r.key ~equal:String.equal in
       Bool.equal r.on_current_stack on));
  check "display depths" ((List.find_exn rows ~f:(fun r -> String.equal r.key "b3")).depth = 3)
;;

let () =
  (* Unfold the codex group: members appear under the folder row,
     selection stays on the folder by key. *)
  let m = psel "main//codex" in
  let m = PS.apply_action ~branches m (PS.Action.Unfold { height = 10 }) in
  let rows = PS.visible ~branches ~overrides:m.overrides in
  check
    "unfolded group shows members under it"
    (List.equal
       String.equal
       (keys rows)
       [ "main"; "b1"; "b2"; "b3"; "old"; "main//codex"; "codex/one"; "codex/two"; "lone" ]);
  check "selection stayed on the folder"
    ([%equal: string option] (PS.selection_key m) (Some "main//codex"));
  (* Fold b1: hides the whole current chain under it (explicit override
     beats the contains-current default). *)
  let m2 = psel "b1" in
  let m2 = PS.apply_action ~branches m2 (PS.Action.Fold { height = 10 }) in
  let rows2 = PS.visible ~branches ~overrides:m2.overrides in
  check
    "folding a chain branch hides its subtree"
    (List.equal
       String.equal
       (keys rows2)
       [ "main"; "b1"; "old"; "main//codex"; "lone" ]);
  check "folded row counts its hidden branches"
    ((List.find_exn rows2 ~f:(fun r -> String.equal r.key "b1")).hidden = 2);
  check "selection stayed on the folded row"
    ([%equal: string option] (PS.selection_key m2) (Some "b1"))
;;

let () =
  (* Fold on a leaf jumps to the parent; wheel scrolls without moving the
     selection; totality on empty. *)
  let m = psel "lone" in
  let m = PS.apply_action ~branches m (PS.Action.Fold { height = 10 }) in
  check "fold on a leaf jumps to the parent"
    ([%equal: string option] (PS.selection_key m) (Some "main"));
  let m = PS.apply_action ~branches PS.Model.initial (PS.Action.Wheel { dir = 1; height = 3 }) in
  check "wheel scrolls the viewport only" (PS.scroll m = 3 && Option.is_none (PS.selection_key m));
  let e = PS.apply_action ~branches:[] PS.Model.initial (PS.Action.Move { dir = `Down; height = 5 }) in
  check "empty stack is total" (Option.is_none (PS.selection_key e) && PS.scroll e = 0)
;;

(* Selection survives the reorders the recency sort produces, and repairs
   to a successor when the branch disappears. *)
let () =
  let m = psel "old" in
  let m = PS.apply_action ~branches m PS.Action.Rows_changed in
  check "selection sticks across a resort"
    ([%equal: string option] (PS.selection_key m) (Some "old"));
  let reordered =
    (* old moved: same rows, different sibling order (lone before old) *)
    List.filter branches ~f:(fun b -> not (String.equal b.name "old"))
  in
  let m2 = PS.apply_action ~branches:reordered m PS.Action.Rows_changed in
  check "deleted branch flows to its successor"
    ([%equal: string option] (PS.selection_key m2) (Some "main//codex"))
;;

let () = print_endline "All stack tests passed."
