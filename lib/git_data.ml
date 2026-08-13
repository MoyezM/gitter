open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* App-root git state, per the hoisting pattern: the root owns cross-leaf
   data (status entries, each section pane's tree state, the derived
   selection) and hands leaves the values plus injects. Leaves keep only
   presentation.

   The Staged and Changes panes are independent [Panes.Files.State]
   instances over the split entries; the SELECTION follows whichever pane
   acted last (its cursor's file feeds the diff).

   Loading: fetch fires whenever the state is [Not_loaded] (guarded by a
   [Loading] marker so it fires once), so [refresh] is just "reset to
   Not_loaded". *)

module Load = struct
  type t =
    | Not_loaded
    | Loading
    | Loaded of (Git.Status.Entry.t list * Git.Status.Branch.t option) Or_error.t
end

let entries_of_load (load : Load.t) =
  match load with
  | Loaded (Ok (entries, _)) -> entries
  | Not_loaded | Loading | Loaded (Error _) -> []
;;

(* Per-file +/- line counts per side; the panes sum them for the title
   bars and show them per row. *)
type diffstat =
  { staged_lines : (int * int) String.Map.t
  ; unstaged_lines : (int * int) String.Map.t
  }

let no_diffstat = { staged_lines = String.Map.empty; unstaged_lines = String.Map.empty }

type t =
  { load : Load.t Bonsai.t (* the stack pane's loading gate *)
  ; refresh : unit Effect.t Bonsai.t
  ; staged : Panes.Files.Component.Input.t
      (* each section pane's FULLY assembled input — status, rows,
         counts, review marks, search surface, inject. The app only adds
         chrome (titles) around these. *)
  ; unstaged : Panes.Files.Component.Input.t
  ; committed : Panes.Files.Component.Input.t
  ; review_progress : (int * int) Bonsai.t (* reviewed, total *)
  ; base : string option Bonsai.t
      (* the committed view's base branch: the Enter-chosen override when
         that branch still exists, else the current branch's inferred
         parent *)
  ; set_base : (string -> unit Effect.t) Bonsai.t
      (* the stack pane's Enter — choosing the current branch clears back
         to the inferred parent *)
  ; selection : Panes.Diff.Fetch.key option Bonsai.t
      (* the active pane's file, tagged with its side — the diff pane shows
         index-vs-HEAD for Staged, worktree-vs-index for Unstaged, and
         merge-base-vs-HEAD for Committed *)
  ; revision : int Bonsai.t
      (* bumped by every index mutation and refresh: content changed even
         though the selection didn't — refetch the diff *)
  ; stage_hunk : (path:string -> raw:string -> unit Effect.t) Bonsai.t
  ; unstage_hunk : (path:string -> raw:string -> unit Effect.t) Bonsai.t
  ; commit_prompt : unit Effect.t Bonsai.t
      (* the commit-message modal, already composed over the git commit
         op — the same effect the sections' [c] key schedules *)
  ; branch : Git.Status.Branch.t option Bonsai.t
  ; stack : Git.Branch_stack.Branch.t list Or_error.t Bonsai.t
      (* the inferred branch stack; Ok [] until first load / no branches *)
  }

(* The effectful keys one section can run, resolved per row at apply
   time. [commit] (targetless: opens the app's commit prompt) and
   [claim] (makes this section's cursor feed the diff pane) are
   section-invariant — [section] fills them in; the per-section
   builders supply only what varies and leave the defaults. *)
type ops =
  { stage : string -> unit Effect.t
  ; unstage : string -> unit Effect.t
  ; discard : string -> unit Effect.t
  ; copy_path : string -> unit Effect.t
  ; toggle_review : string -> unit Effect.t
  ; commit : unit Effect.t
  ; claim : unit Effect.t
  }

(* [discard_confirm] wraps the worktree-destructive discard in the app's
   confirm modal; [copy_path] is the app's clipboard effect (both live at
   the app root: the modal and the tty are the app's). *)
(* [set_notice] posts error notices to the app's status bar (the app owns
   the notice state so it can also flash neutral info like "copied"). *)
let create
      ~(discard_confirm : (path:string -> discard:unit Effect.t -> unit Effect.t) Bonsai.t)
      ~(commit_confirm : (commit:(string -> unit Effect.t) -> unit Effect.t) Bonsai.t)
      ~(copy_path : (string -> unit Effect.t) Bonsai.t)
      ~(set_notice : (string option -> unit Effect.t) Bonsai.t)
      (local_ graph)
  =
  let load, set_load = Bonsai.state Load.Not_loaded graph in
  (* Latest-wins: refresh mid-flight starts a second fetch; only the
     NEWEST may land, or a slow stale status overwrites a fresh one. *)
  let status_latest = Latest.create () in
  (* The poller's last-seen change signature; None means "baseline the next
     poll silently" (set after every explicit fetch, so a user action isn't
     followed by a redundant poll-triggered refresh). *)
  let poll_signature : string option ref = ref None in
  let diffstat, set_diffstat = Bonsai.state no_diffstat graph in
  (* THE status landing: everything that fetches status lands it (and its
     diffstat) through here, atomically. Deliberately no intermediate
     state transition: a refresh keeps SHOWING the old entries until the
     new ones land — flashing "loading" on every stage/unstage reads as
     whole-screen flicker. *)
  let land_status =
    let%arr set_load and set_diffstat in
    fun (result, stat) ->
      Effect.Many
        [ set_load
            (Loaded (Or_error.map result ~f:(fun (_raw, entries, branch) -> entries, branch)))
        ; set_diffstat
            (match stat with
             | Ok (staged_lines, unstaged_lines) -> { staged_lines; unstaged_lines }
             | Error (_ : Error.t) -> no_diffstat)
        ]
  in
  let fetch_now =
    let%arr land_status in
    Latest.run
      status_latest
      ~on_start:(fun () -> poll_signature := None)
      ~fetch:(fun () ->
        let open Async in
        Deferred.both (Git.Queries.status ()) (Git.Queries.diffstat ()))
      ~commit:land_status
  in
  (* The inferred branch stack, refreshed alongside the status (mutations
     and the poller both go through [refresh]). *)
  let stack, set_stack = Bonsai.state (Ok []) graph in
  let stack_latest = Latest.create () in
  let fetch_stack =
    let%arr set_stack in
    Latest.run stack_latest ~fetch:Git.Branch_stack.fetch ~commit:set_stack
  in
  (* The committed view: this branch vs its inferred base (the stack's
     parent of current). Entries + per-file counts land together; a base
     change refetches via the on_change below. *)
  let base_override, set_base_override = Bonsai.state None graph in
  let base =
    let%arr stack and base_override in
    let branches =
      match stack with
      | Ok branches -> branches
      | Error (_ : Error.t) -> []
    in
    match base_override with
    | Some b
      when List.exists branches ~f:(fun (br : Git.Branch_stack.Branch.t) ->
             String.equal br.name b && not br.is_current) -> Some b
    | Some _ (* the chosen branch vanished or became current: fall back *)
    | None -> Git.Branch_stack.parent_of_current branches
  in
  let set_base =
    let%arr set_base_override in
    fun branch -> set_base_override (Some branch)
  in
  let committed_state, set_committed =
    Bonsai.state ([], String.Map.empty, String.Map.empty) graph
  in
  let committed_latest = Latest.create () in
  let fetch_committed =
    let%arr set_committed and base in
    Latest.run
      committed_latest
      ~fetch:(fun () ->
        match base with
        | None -> Async.return (Ok ([], String.Map.empty, String.Map.empty))
        | Some base -> Git.Queries.committed ~base ())
      ~commit:(fun result ->
        set_committed
          (match result with
           | Ok r -> r
           | Error (_ : Error.t) -> [], String.Map.empty, String.Map.empty))
  in
  Bonsai.Edge.on_change
    ~equal:[%equal: string option]
    ~callback:
      (let%arr fetch_committed in
       fun (_ : string option) -> fetch_committed)
    base
    graph;
  (* Review marks, keyed by the content pair (old blob, new blob) — the
     spec's design: a mark survives restacks that don't touch the file and
     self-invalidates the moment either side's content changes (a stale
     mark simply matches nothing; no invalidation logic exists anywhere).
     In-memory set here; the store loads/persists it. *)
  let marks, set_marks = Bonsai.state String.Set.empty graph in
  let mark_key (old_blob, new_blob) = old_blob ^ ":" ^ new_blob in
  let load_marks =
    let%arr set_marks and set_notice in
    let%bind.Effect stored = Effect.of_deferred_thunk (fun () -> Review_store.load ()) in
    match stored with
    | Ok pairs -> set_marks (String.Set.of_list (List.map pairs ~f:mark_key))
    | Error e ->
      set_notice (Some (String.prefix (Error.to_string_hum e) 80))
  in
  (* Only the FIRST load shows the loading message. *)
  let fetch =
    let%arr load and set_load and fetch_now and fetch_stack and load_marks in
    match load with
    | Load.Not_loaded ->
      Effect.Many [ set_load Loading; fetch_now; fetch_stack; load_marks ]
    | Loading | Loaded _ -> Effect.Ignore
  in
  Bonsai.Edge.before_display fetch graph;
  let revision, bump_revision =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ctx revision () -> revision + 1)
      graph
  in
  (* Every refresh bumps [revision] (the diff pane's "your content may be
     stale, refetch" signal) as it reloads status. This is load-bearing:
     the poller's own [`Changed] bump can be dropped when a concurrent
     refresh (e.g. closing the terminal overlay) supersedes its status
     landing mid-fetch — so the refresh that WON the race must carry the
     bump, or the open diff renders pre-change content indefinitely. A
     redundant bump (content actually unchanged) costs one refetch that
     keeps cursor and scroll. *)
  let refresh =
    let%arr fetch_now and fetch_stack and fetch_committed and bump_revision in
    Effect.Many [ bump_revision (); fetch_now; fetch_stack; fetch_committed ]
  in
  (* An index mutation, then resync: the status reloads and [revision] tells
     the diff pane its content is stale. Failures surface in the status bar
     (first line only) and clear on the next success; the refresh runs
     either way — it IS the recovery (and now carries the revision bump). *)
  let mutate =
    let%arr refresh and set_notice in
    fun op ->
      let%bind.Effect result = Effect.of_deferred_thunk op in
      let notice =
        match result with
        | Ok () -> None
        | Error e ->
          (match String.split_lines (Error.to_string_hum e) with
           | first :: _ -> Some (String.prefix first 80)
           | [] -> Some "git command failed")
      in
      Effect.Many [ set_notice notice; refresh ]
  in
  let stage_path =
    let%arr mutate in
    fun path -> mutate (fun () -> Git.Stage.stage_path path)
  in
  let unstage_path =
    let%arr mutate in
    fun path -> mutate (fun () -> Git.Stage.unstage_path path)
  in
  let stage_hunk =
    let%arr mutate in
    fun ~path ~raw -> mutate (fun () -> Git.Stage.stage_hunk ~path ~raw)
  in
  let unstage_hunk =
    let%arr mutate in
    fun ~path ~raw -> mutate (fun () -> Git.Stage.unstage_hunk ~path ~raw)
  in
  let discard_path =
    let%arr mutate in
    fun path -> mutate (fun () -> Git.Worktree.discard_path path)
  in
  let commit =
    let%arr mutate in
    fun message -> mutate (fun () -> Git.Commit.run ~message)
  in
  (* The commit-message prompt, as a section op: [c] routes through the
     state machine like every other effectful key, so a same-frame
     "/c" burst resolves against the CURRENT prompt state instead of
     opening the modal over a half-typed query. *)
  let commit_prompt =
    let%arr commit_confirm and commit in
    commit_confirm ~commit
  in
  let committed_counts =
    let%arr committed_state in
    let _, counts, _ = committed_state in
    counts
  in
  let committed_blobs =
    let%arr committed_state in
    let _, _, blobs = committed_state in
    blobs
  in
  (* Reviewed DISPLAY keys: file paths whose pair is marked, plus dir
     paths whose entire subtree is marked (so collapsed dirs read
     correctly). *)
  let reviewed =
    let%arr committed_blobs and marks in
    let reviewed_file path =
      match Map.find committed_blobs path with
      | Some pair -> Set.mem marks (mark_key pair)
      | None -> false
    in
    let files = Map.keys committed_blobs in
    let dirs =
      List.concat_map files ~f:(fun path ->
        let parts = String.split path ~on:'/' in
        List.init
          (List.length parts - 1)
          ~f:(fun i -> String.concat ~sep:"/" (List.take parts (i + 1))))
      |> List.dedup_and_sort ~compare:String.compare
    in
    let reviewed_dirs =
      List.filter dirs ~f:(fun d ->
        let prefix = d ^ "/" in
        let under = List.filter files ~f:(String.is_prefix ~prefix) in
        (not (List.is_empty under)) && List.for_all under ~f:reviewed_file
      )
    in
    String.Set.of_list (List.filter files ~f:reviewed_file @ reviewed_dirs)
  in
  let review_progress =
    let%arr committed_blobs and marks in
    let total = Map.length committed_blobs in
    let done_ =
      Map.count committed_blobs ~f:(fun pair -> Set.mem marks (mark_key pair))
    in
    done_, total
  in
  (* Toggle a file, or a whole directory (all-marked folds back to none —
     the same all-or-nothing rule as staging a dir). *)
  let toggle_review =
    let%arr marks and set_marks and set_notice and committed_blobs in
    fun path ->
      let keys =
        match Map.find committed_blobs path with
        | Some pair -> [ mark_key pair ]
        | None ->
          let prefix = path ^ "/" in
          Map.fold committed_blobs ~init:[] ~f:(fun ~key ~data acc ->
            if String.is_prefix key ~prefix then mark_key data :: acc else acc)
      in
      match keys with
      | [] -> Effect.Ignore
      | _ ->
        let all_marked = List.for_all keys ~f:(Set.mem marks) in
        let marks =
          if all_marked
          then List.fold keys ~init:marks ~f:Set.remove
          else List.fold keys ~init:marks ~f:Set.add
        in
        let pairs =
          List.filter_map keys ~f:(fun k -> String.lsplit2 k ~on:':')
        in
        (* Optimistic: the set updates now, the store catches up; a store
           failure surfaces in the status bar (marks revert on restart —
           honest about what persisted). *)
        Effect.Many
          [ set_marks marks
          ; (let%bind.Effect written =
               Effect.of_deferred_thunk (fun () ->
                 Review_store.set ~pairs ~reviewed:(not all_marked))
             in
             match written with
             | Ok () -> Effect.Ignore
             | Error e -> set_notice (Some (String.prefix (Error.to_string_hum e) 80)))
          ]
  in
  (* Whichever pane acted last owns the selection. *)
  let active, set_active = Bonsai.state `Unstaged graph in
  (* Per-section effectful-key handlers. They ride the state machine
     INPUT so [Operate] resolves its target against the model at APPLY
     time (burst-safe — see Panes.Files.State.Op); the wrapper schedules
     the matching effect. *)
  let noop_op (_ : string) = Effect.Ignore in
  let noop_ops =
    { stage = noop_op
    ; unstage = noop_op
    ; discard = noop_op
    ; copy_path = noop_op
    ; toggle_review = noop_op
    ; commit = Effect.Ignore
    ; claim = Effect.Ignore
    }
  in
  (* One section pane, machine to assembled [Input]: the tree state
     machine, its phys-stable projections, and the pane's full input
     record ([which] doubles as [Input.side]). Also returns the derived
     file selection that feeds the diff. *)
  let section ~which ~entries ~ops ~status ~counts ~reviewed ~hints =
    (* The section-invariant ops ([claim], [commit]) are stamped here;
       the per-section builders supply only what varies. *)
    let input =
      let%arr entries and ops and set_active and commit = commit_prompt in
      entries, { ops with claim = set_active which; commit }
    in
    let model, inject =
      Bonsai.state_machine_with_input
        ~default_model:Panes.Files.State.Model.initial
        ~apply_action:(fun ctx input model action ->
          let entries, ops =
            match input with
            | Bonsai.Computation_status.Active input -> input
            | Inactive -> [], noop_ops
          in
          let schedule e = Bonsai.Apply_action_context.schedule_event ctx e in
          (* The key's mode-dependent reading resolves against the
             CURRENT model (burst-safe), and so does whether the action
             claims the diff pane. *)
          (* Which actions claim the diff pane for this section: cursor
             motions, opening the prompt, a jump that will land — n with
             no live match is a no-op and must not flip which pane feeds
             the diff (keys table). Decided here, at apply time. *)
          let claims (action : Panes.Files.State.Action.t Search.Action.t) =
            match action with
            | Search.Action.Pane (Nav (Move _ | Activate _ | Fold _ | Unfold _))
            | Prompt { event = Search.Prompt.Open; _ } -> true
            | Jump _ ->
              Panes.Files.State.can_jump ~entries model.Search.Tree_search.Model.search
            | Pane (Nav (Wheel _ | Rows_changed) | Operate _ | Commit_prompt)
            | Prompt _ | By_mode _ -> false
          in
          match Search.Action.resolve ~search:model.Search.Tree_search.Model.search action with
          | None -> model
          | Some action ->
            if claims action then schedule ops.claim;
            (match action with
             | Search.Action.Pane Panes.Files.State.Action.Commit_prompt ->
               schedule ops.commit;
               model
             | Pane (Operate op) ->
               (match Panes.Files.State.target ~entries model with
                | None -> ()
                | Some path ->
                  let run =
                    match op with
                    | Panes.Files.State.Op.Stage -> ops.stage
                    | Unstage -> ops.unstage
                    | Discard -> ops.discard
                    | Copy_path -> ops.copy_path
                    | Toggle_review -> ops.toggle_review
                  in
                  schedule (run path));
               model
             | action -> Panes.Files.State.apply_action ~entries model action))
        input
        graph
    in
    (* Field projections are phys-stable across listing-only updates, so
       everything derived from them — the rows, the repair edge's key
       list, the border counts — sits out plain navigation (a trackpad
       flick delivers wheel events at frame rate; the tree must not
       re-sort per tick). *)
    let collapsed =
      let%arr model in
      model.Search.Tree_search.Model.fold
    in
    let search =
      let%arr model in
      model.Search.Tree_search.Model.search
    in
    let rows =
      let%arr entries and collapsed and search in
      Panes.Files.State.displayed_rows ~entries ~collapsed ~search
    in
    (* Selection is a key; the repair transition runs whenever the rows'
       keys change (refresh, stage/unstage, external mutations — and,
       while a search is active, filter churn: the transition holds
       still when an active search narrows to nothing). *)
    Bonsai.Edge.on_change
      ~equal:[%equal: string list]
      ~callback:
        (let%arr inject in
         fun (_ : string list) ->
           inject (Search.Action.pane (Panes.Files.State.Action.Nav Tree_listing.Action.Rows_changed)))
      (let%arr rows in
       List.map rows ~f:Panes.Files.State.row_key)
      graph;
    (* [selection_key]/[pre_prompt] are phys-stable across wheel ticks
       ([Listing.wheel] touches only [scroll]), so the cursor and the
       diff-feeding selection sit out scroll churn like everything else
       here — a whole-model dependency would recompute them (and break
       the diff title's cutoff) at trackpad frame rate. *)
    let selection_key =
      let%arr model in
      Panes.Files.State.selection_key model
    in
    let pre_prompt =
      let%arr model in
      Search.Tree_search.pre_prompt_selection model
    in
    let cursor =
      let%arr rows and selection_key in
      Panes.Files.State.index_of rows selection_key
    in
    let scroll =
      let%arr model in
      Panes.Files.State.scroll model
    in
    let search_counts =
      (* entries change on refresh, search on query edits — the counter
         must not re-sort the tree on every cursor move. *)
      let%arr entries and search in
      Panes.Files.State.match_counts ~entries search
    in
    let selection =
      (* The file feeding the diff pane; [effective_selection] owns the
         T5 zero-match fallback. *)
      let%arr rows and entries and search and collapsed and pre_prompt and selection_key in
      Panes.Files.State.effective_selection
        ~entries
        ~rows
        ~search
        ~fold:collapsed
        ~pre_prompt
        ~selection:selection_key
    in
    ( { Panes.Files.Component.Input.status
      ; rows
      ; cursor
      ; scroll
      ; counts
      ; reviewed
      ; side = which
      ; search
      ; search_counts
      ; inject
      ; hints
      }
    , selection )
  in
  let filtered filter =
    let%arr load in
    List.filter (entries_of_load load) ~f:filter
  in
  (* The tree-vs-idle-message decision (T5) is the PANE's, derived from
     its own rows+search — status only says what loaded and what the
     idle message would be. *)
  let files_status ~empty =
    let%arr load in
    match load with
    | Load.Not_loaded | Loading -> `Loading
    | Loaded (Error e) -> `Error e
    | Loaded (Ok _) -> `Loaded empty
  in
  let committed_status =
    let%arr load and base in
    match load with
    | Load.Not_loaded | Loading -> `Loading
    | Loaded _ ->
      (match base with
       | None -> `Empty "no base branch"
       | Some b -> `Loaded ("nothing committed vs " ^ b))
  in
  let no_reviews = Bonsai.return String.Set.empty in
  let staged, staged_selection =
    section
      ~which:`Staged
      ~entries:(filtered Git.Status.Entry.is_staged)
      ~ops:
        (let%arr unstage = unstage_path
         and copy_path in
         { noop_ops with unstage; copy_path })
      ~status:(files_status ~empty:"nothing staged")
      ~counts:
        (let%arr diffstat in
         diffstat.staged_lines)
      ~reviewed:no_reviews
      ~hints:"j/k:move  u:unstage  c:commit  /:search  y:copy path"
  in
  let unstaged, unstaged_selection =
    section
      ~which:`Unstaged
      ~entries:(filtered Git.Status.Entry.is_unstaged)
      ~ops:
        (let%arr stage = stage_path
         and discard_path
         and discard_confirm
         and copy_path in
         { noop_ops with
           stage
         ; copy_path
         ; discard = (fun path -> discard_confirm ~path ~discard:(discard_path path))
         })
      ~status:(files_status ~empty:"working tree clean")
      ~counts:
        (let%arr diffstat in
         diffstat.unstaged_lines)
      ~reviewed:no_reviews
      ~hints:"j/k:move  s:stage  d:discard  c:commit  /:search  y:copy path"
  in
  let committed, committed_selection =
    section
      ~which:`Committed
      ~entries:
        (let%arr committed_state in
         let entries, _, _ = committed_state in
         entries)
      ~ops:
        (let%arr copy_path and toggle_review in
         { noop_ops with copy_path; toggle_review })
      ~status:committed_status
      ~counts:committed_counts
      ~reviewed
      ~hints:"j/k:move  r:reviewed  c:commit  /:search  y:copy path"
  in
  let selection =
    let%arr active
    and staged_selection
    and unstaged_selection
    and committed_selection
    and base in
    match active with
    | `Staged -> Option.map staged_selection ~f:(fun p -> p, `Staged)
    | `Unstaged -> Option.map unstaged_selection ~f:(fun p -> p, `Unstaged)
    | `Committed ->
      Option.both committed_selection base
      |> Option.map ~f:(fun (p, b) -> p, `Committed b)
  in
  let branch =
    let%arr load in
    match load with
    | Load.Loaded (Ok (_, branch)) -> branch
    | Not_loaded | Loading | Loaded (Error _) -> None
  in
  (* External changes (editor saves, git commands in another terminal):
     poll a cheap signature every 2s — the raw status output plus the
     mtimes of .git/index and the selected worktree file (content edits to
     an already-listed file change no status line, but must refresh the
     open diff). Only a real change swaps state and bumps the revision, so
     idle polls cost one subprocess and zero re-renders. Polls yield to
     explicit fetches via the generation check. *)
  let peek_selection = Bonsai.peek selection graph in
  let poll =
    let%arr land_status and bump_revision and peek_selection and fetch_stack and fetch_committed in
    let%bind.Effect mine = Latest.observe status_latest in
    let%bind.Effect selected = peek_selection in
    let selected_path =
      match selected with
      | Bonsai.Computation_status.Active (Some (path, _)) -> Some path
      | Active None | Inactive -> None
    in
    let%bind.Effect outcome =
      Effect.of_deferred_thunk (fun () ->
        let open Async in
        let mtime path =
          match%map Monitor.try_with (fun () -> Unix.stat path) with
          | Ok stats ->
            Float.to_string (Time_float.Span.to_sec (Time_float.to_span_since_epoch stats.mtime))
          | Error _ -> "absent"
        in
        let%bind status = Git.Queries.status () in
        let%bind index_m = mtime ".git/index" in
        (* Ref moves on OTHER branches (restacks, new branches) change
           neither the status output nor the index — the refs listing is
           what makes the stack pane track them. *)
        let%bind refs = Git.Queries.refs_signature () in
        let%map selected_m =
          match selected_path with
          | Some path -> mtime path
          | None -> return "-"
        in
        let raw =
          match status with
          | Ok (raw, _, _) -> raw
          | Error e -> "error:" ^ Error.to_string_hum e
        in
        status, String.concat ~sep:"|" [ raw; index_m; selected_m; refs ])
    in
    let status, signature = outcome in
    let%bind.Effect decision =
      Effect.of_thunk (fun () ->
        if Latest.peek status_latest <> mine
        then `Skip (* a user action interleaved; its fetch owns the state *)
        else (
          match !poll_signature with
          | None ->
            poll_signature := Some signature;
            `Skip (* baseline after an explicit fetch *)
          | Some prev when String.equal prev signature -> `Skip
          | Some _ ->
            poll_signature := Some signature;
            `Changed))
    in
    match decision with
    | `Skip -> Effect.Ignore
    | `Changed ->
      (* An external change usually moved the index too: fetch a fresh
         diffstat and land BOTH through the one landing path — the poller
         silently keeping stale +/- counts after an external [git add]
         was a live bug. Re-check currency after the extra fetch. *)
      let%bind.Effect stat = Effect.of_deferred_thunk (fun () -> Git.Queries.diffstat ()) in
      Latest.when_current
        status_latest
        mine
        (Effect.Many
           [ land_status (status, stat); bump_revision (); fetch_stack; fetch_committed ])
  in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Wait_period_after_previous_effect_finishes_blocking
    ~trigger_on_activate:false
    (Bonsai.return (Time_ns.Span.of_sec 2.))
    poll
    graph;
  { load
  ; refresh
  ; staged
  ; unstaged
  ; committed
  ; review_progress
  ; base
  ; set_base
  ; selection
  ; revision
  ; stage_hunk
  ; unstage_hunk
  ; commit_prompt
  ; branch
  ; stack
  }
;;
