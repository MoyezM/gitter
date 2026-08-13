open! Core
open! Bonsai_term

module Model = struct
  type 'fold t =
    { listing : Listing.Model.t
    ; fold : 'fold
    ; search : Prompt.t
    ; snapshot : (Listing.Model.t * 'fold) option
    }

  let initial fold =
    { listing = Listing.Model.initial; fold; search = Prompt.idle; snapshot = None }
  ;;
end

type ('row, 'fold) pane =
  { rows : 'fold -> 'row list
  ; all_rows : unit -> 'row list
  ; key : 'row -> string
  ; depth : 'row -> int
  ; is_parent : 'row -> bool
  ; candidate : 'row -> string option
  ; reveal : 'fold -> key:string -> 'fold
  }

let is_match pane query row =
  match pane.candidate row with
  | Some text -> Query.matches query text
  | None -> false
;;

(* Narrowing (T1, T2): the rows the query keeps, plus their ancestor
   chain — walk with a stack of the open parents; a kept row marks them
   all, exactly its ancestors at that point of the DFS. *)
let narrow pane rows ~query =
  let rows = Array.of_list rows in
  let kept = Array.map rows ~f:(is_match pane query) in
  let stack = ref [] in
  Array.iteri rows ~f:(fun i r ->
    let d = pane.depth r in
    stack := List.filter !stack ~f:(fun (pd, _) -> pd < d);
    if kept.(i) then List.iter !stack ~f:(fun (_, j) -> kept.(j) <- true);
    if pane.is_parent r then stack := (d, i) :: !stack);
  Array.filteri rows ~f:(fun i _ -> kept.(i)) |> Array.to_list
;;

let displayed_rows pane ~fold ~(search : Prompt.t) =
  match search.typed with
  | Some typed ->
    let query = Query.parse typed in
    if Query.is_empty query then pane.all_rows () else narrow pane (pane.all_rows ()) ~query
  | None -> pane.rows fold
;;

let match_counts pane (search : Prompt.t) =
  match Prompt.parsed search with
  | None -> 0, 0
  | Some query ->
    let all = pane.all_rows () in
    List.count all ~f:(is_match pane query), List.length all
;;

let can_jump pane (search : Prompt.t) =
  match Prompt.match_query search with
  | None -> false
  | Some query -> List.exists (pane.all_rows ()) ~f:(is_match pane query)
;;

let border_of_counts ~search ~counts:(matches, total) ~width =
  Border.view ~prompt:search ~counts:(Some (sprintf "%d/%d" matches total)) ~width
;;

let border pane ~search ~width =
  border_of_counts ~search ~counts:(match_counts pane search) ~width
;;

let underline ~query ~attrs ~candidate label =
  let spans =
    match query with
    | None -> []
    | Some query -> Option.value (Query.spans query candidate) ~default:[]
  in
  Query.runs ~spans ~offset:(String.length candidate - String.length label) label
  |> List.map ~f:(fun (text, matched) ->
    View.text ~attrs:(if matched then Attr.underline :: attrs else attrs) text)
;;

let index_of pane rows selection = Listing.index_of ~key:pane.key rows selection

let pre_prompt_selection (m : _ Model.t) =
  match m.snapshot with
  | Some (listing, _) -> listing.Listing.Model.selection
  | None -> m.listing.selection
;;

let select ?height pane rows ~index (m : 'fold Model.t) =
  { m with Model.listing = Listing.select ~key:pane.key rows ?height ~index m.listing }
;;

(* Re-select after the query changed (T4): the first MATCHING row in
   tree order — ancestors don't count. At zero matches (and on the
   empty query) the pre-prompt selection remains the effective one
   (T5). *)
let live_select pane ~height (m : 'fold Model.t) =
  let rows = displayed_rows pane ~fold:m.fold ~search:m.search in
  let first_match =
    match Prompt.match_query m.search with
    | None -> None
    | Some query -> List.findi rows ~f:(fun _ r -> is_match pane query r) |> Option.map ~f:fst
  in
  match first_match with
  | Some index -> select pane rows ~height ~index m
  | None ->
    let selection = pre_prompt_selection m in
    let listing = { m.listing with Listing.Model.selection } in
    if List.is_empty rows || Option.is_none selection
    then { m with Model.listing = listing }
    else
      select pane rows ~height ~index:(index_of pane rows selection) { m with Model.listing = listing }
;;

(* Reveal whatever hides [key], then select it — commits keep the
   accepted selection (T6), jumps land on matches (T7); the reveals are
   ordinary fold changes, kept until the user folds them. *)
let reveal_key pane ~height (m : 'fold Model.t) key =
  let fold = pane.reveal m.fold ~key in
  let rows = pane.rows fold in
  select pane rows ~height ~index:(index_of pane rows (Some key)) { m with Model.fold = fold }
;;

let apply_prompt pane (m : 'fold Model.t) (event : Prompt.event) ~height =
  let was_active = Prompt.is_active m.search in
  let search = Prompt.apply m.search event in
  let m' = { m with Model.search = search } in
  match event with
  | Prompt.Open ->
    if was_active
    then m'
    else (
      (* Snapshot what Esc restores (L4), then reveal the selection in
         the bypassed (fully expanded) tree the prompt shows. *)
      let m' = { m' with Model.snapshot = Some (m.listing, m.fold) } in
      let rows = displayed_rows pane ~fold:m'.fold ~search:m'.search in
      if List.is_empty rows
      then m'
      else select pane rows ~height ~index:(index_of pane rows m'.listing.selection) m')
  | Type _ | Backspace | Recall_prev | Recall_next ->
    if was_active then live_select pane ~height m' else m'
  | Commit | Implicit_commit ->
    if not was_active
    then m'
    else (
      let m' = { m' with Model.snapshot = None } in
      match m'.listing.selection with
      | Some key -> reveal_key pane ~height m' key
      | None -> m')
  | Cancel ->
    (* Restore exactly what the prompt opened over (L4); a concurrent
       refresh makes this best-effort — the pane's next rows-changed
       repair re-anchors a restored selection whose row vanished. *)
    (match m.snapshot with
     | Some (listing, fold) -> { Model.search; snapshot = None; listing; fold }
     | None -> { m' with Model.snapshot = None })
  | Clear -> m'
;;

let jump pane (m : 'fold Model.t) ~dir ~height =
  match Prompt.match_query m.search with
  | None -> m
  | Some query ->
    let all = pane.all_rows () in
    let matches =
      List.filter_mapi all ~f:(fun i r -> if is_match pane query r then Some i else None)
      |> Array.of_list
    in
    let current = index_of pane all m.listing.selection in
    (match Positions.next ~dir ~current matches with
     | None -> m
     | Some target -> reveal_key pane ~height m (pane.key (List.nth_exn all target)))
;;

let apply pane ~apply_pane (m : 'fold Model.t) (action : _ Action.t) =
  match Action.resolve ~search:m.search action with
  | None -> m
  | Some (Action.Pane a) -> apply_pane m a
  | Some (Prompt { event; height }) -> apply_prompt pane m event ~height
  | Some (Jump { dir; height }) -> jump pane m ~dir ~height
  | Some (By_mode _) -> m (* [resolve] never returns one *)
;;
