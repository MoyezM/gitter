open! Core

(* Selection, collapse, viewport and search state over ONE file tree,
   pure. The selection/viewport laws live in [Listing] (shared with the
   stack pane); the search lifecycle in [Search.Tree_search] — this
   module supplies its pane record and keeps what is files-specific:
   the dir/file activation semantics and the effectful ops.

   [Rows_changed] is injected by the host whenever the derived rows'
   keys change (refreshes, external mutations, filter churn). *)

module Model = struct
  (* fold = the collapsed-directory set (full paths). *)
  type t = String.Set.t Search.Tree_search.Model.t

  let initial : t = Search.Tree_search.Model.initial String.Set.empty
end

module Op = struct
  (* The effectful keys. Routed through the state machine so the TARGET
     resolves against the model at APPLY time — a same-frame [j, s] burst
     must stage the row j moved to, not a stale snapshot (the codebase's
     recurring burst-collapse class). The host schedules the actual
     effect via [Apply_action_context.schedule_event]. *)
  type t =
    | Stage
    | Unstage
    | Discard
    | Copy_path
    | Toggle_review
  [@@deriving sexp_of]
end

module Action = struct
  (* The navigation vocabulary is [Tree_listing]'s; only what is
     files-specific stays here — the effectful ops. *)
  type t =
    | Nav of Tree_listing.Action.t
    | Operate of Op.t (* handled by the HOST's apply_action wrapper *)
    | Commit_prompt (* [c]: targetless, so not an [Op]; host schedules it *)
  [@@deriving sexp_of]
end

let row_key (row : Tree.row) =
  match row with
  | Tree.Dir { path; _ } -> path
  | File { entry; _ } -> entry.Git.Status.Entry.path
;;

let selection_key (model : Model.t) = model.listing.selection
let scroll (model : Model.t) = model.listing.scroll
let index_of rows selection = Listing.index_of ~key:row_key rows selection

(* The proper directory prefixes of a path — what revealing it must
   expand (T6, T7). *)
let ancestor_set path =
  String.split path ~on:'/'
  |> List.drop_last_exn
  |> List.folding_map ~init:None ~f:(fun parent seg ->
    let dir =
      match parent with
      | None -> seg
      | Some parent -> parent ^ "/" ^ seg
    in
    Some dir, dir)
  |> String.Set.of_list
;;

(* The five row facts and the reveal that make this pane searchable:
   every row matches on its full path; directories carry descendants. *)
let pane ~entries : (Tree.row, String.Set.t) Search.Tree_search.pane =
  { rows = (fun collapsed -> Tree.rows ~entries ~collapsed)
  ; all_rows = (fun () -> Tree.rows ~entries ~collapsed:String.Set.empty)
  ; key = row_key
  ; depth = Tree.depth
  ; is_parent =
      (function
        | Tree.Dir _ -> true
        | File _ -> false)
  ; candidate = (fun r -> Some (row_key r))
  ; reveal = (fun collapsed ~key -> Set.diff collapsed (ancestor_set key))
  }
;;

let displayed_rows ~entries ~collapsed ~search =
  Search.Tree_search.displayed_rows (pane ~entries) ~fold:collapsed ~search
;;

let visible_rows ~entries (model : Model.t) =
  displayed_rows ~entries ~collapsed:model.fold ~search:model.search
;;

let match_counts ~entries search = Search.Tree_search.match_counts (pane ~entries) search

(* Whether [n]/[N] would actually jump — a register with a live match. *)
let can_jump ~entries search = Search.Tree_search.can_jump (pane ~entries) search

(* What folding means here: directory rows fold into the collapsed-path
   set; files are leaves. *)
let caps ~entries : (Tree.row, String.Set.t) Tree_listing.caps =
  { pane = pane ~entries
  ; fold_state =
      (function
        | Tree.Dir { expanded; _ } -> if expanded then `Open else `Closed
        | File _ -> `Leaf)
  ; set_folded =
      (fun fold ~key ~folded -> if folded then Set.add fold key else Set.remove fold key)
  }
;;

(* The pane's own transitions, over already-RESOLVED actions —
   [Search.Tree_search.apply] owns the mode dispatch and the search
   lifecycle; [Tree_listing.apply] owns navigation. *)
let apply_plain ~entries (model : Model.t) (action : Action.t) =
  match action with
  | Action.Commit_prompt | Operate _ -> model (* the host wrapper schedules the effect *)
  | Nav nav -> Tree_listing.apply (caps ~entries) model nav
;;

let apply_action ~entries (model : Model.t) (action : Action.t Search.Action.t) =
  Search.Tree_search.apply (pane ~entries) ~apply_pane:(apply_plain ~entries) model action
;;

(* The operation target under the CURRENT selection: a file's path, or a
   directory's whole subtree path (git pathspecs make that one op).
   Resolved against the displayed rows — ops only fire idle, where those
   are the folded tree. *)
let target ~entries (model : Model.t) =
  (* Look the selection up BY KEY — [index_of] defaults a missing key to
     row 0, which for an effectful op (stage/unstage/discard) would
     retarget to the wrong file during the brief window before the
     [Rows_changed] repair runs. No matching row ⇒ no target ⇒ the host
     wrapper schedules nothing. *)
  match model.listing.selection with
  | None -> None
  | Some key ->
    let rows = visible_rows ~entries model in
    List.find rows ~f:(fun r -> String.equal (row_key r) key) |> Option.map ~f:row_key
;;

(* The file under the selection, if any — a directory row selects
   nothing. *)
let selection rows ~cursor =
  match List.nth rows cursor with
  | Some (Tree.File { entry; _ }) -> Some entry.Git.Status.Entry.path
  | Some (Dir _) | None -> None
;;

(* The file feeding the diff pane. When an active search narrows the
   visible rows to nothing, the PRE-PROMPT selection remains effective —
   the diff keeps showing it (T5) — resolved against the unfiltered tree
   in that rare branch. Takes model pieces (not the model) so hosts feed
   it phys-stable projections; [pre_prompt] is
   [Search.Tree_search.pre_prompt_selection]. *)
let effective_selection ~entries ~rows ~search ~fold ~pre_prompt ~selection:key =
  match rows with
  | [] when Search.Prompt.is_active search ->
    let rows = Tree.rows ~entries ~collapsed:fold in
    selection rows ~cursor:(index_of rows pre_prompt)
  | rows -> selection rows ~cursor:(index_of rows key)
;;

