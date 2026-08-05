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

(* One section pane's slice: its visible rows, cursor, and inject. *)
type section_data =
  { rows : Panes.Files.Tree.row list Bonsai.t
  ; cursor : int Bonsai.t
  ; inject : (Panes.Files.State.Action.t -> unit Effect.t) Bonsai.t
  }

type t =
  { load : Load.t Bonsai.t
  ; refresh : unit Effect.t Bonsai.t
  ; staged : section_data
  ; unstaged : section_data
  ; selection : (string * [ `Staged | `Unstaged ]) option Bonsai.t
      (* the active pane's file, tagged with its side — the diff pane shows
         index-vs-HEAD for Staged and worktree-vs-index for Unstaged *)
  ; revision : int Bonsai.t
      (* bumped by every index mutation and refresh: content changed even
         though the selection didn't — refetch the diff *)
  ; stage_path : (string -> unit Effect.t) Bonsai.t
  ; unstage_path : (string -> unit Effect.t) Bonsai.t
  ; stage_hunk : (path:string -> raw:string -> unit Effect.t) Bonsai.t
  ; unstage_hunk : (path:string -> raw:string -> unit Effect.t) Bonsai.t
  ; discard_path : (string -> unit Effect.t) Bonsai.t
      (* DESTRUCTIVE (worktree) — the app must gate it behind the confirm
         modal; this is the raw operation *)
  ; commit : (string -> unit Effect.t) Bonsai.t
  ; branch : Git.Status.Branch.t option Bonsai.t
  ; notice : string option Bonsai.t
      (* the last failed mutation's message; cleared by the next success *)
  }

let create (local_ graph) =
  let load, set_load = Bonsai.state Load.Not_loaded graph in
  (* Fetch generation: refresh mid-flight starts a second fetch; only the
     NEWEST fetch may land, or a slow stale status overwrites a fresh one. *)
  let generation = ref 0 in
  (* The poller's last-seen change signature; None means "baseline the next
     poll silently" (set after every explicit fetch, so a user action isn't
     followed by a redundant poll-triggered refresh). *)
  let poll_signature : string option ref = ref None in
  (* Run a status fetch and swap the result in atomically. Deliberately no
     intermediate state transition: a refresh keeps SHOWING the old entries
     until the new ones land — flashing "loading" on every stage/unstage
     reads as whole-screen flicker. *)
  let fetch_now =
    let%arr set_load in
    let%bind.Effect mine =
      Effect.of_thunk (fun () ->
        incr generation;
        poll_signature := None;
        !generation)
    in
    let%bind.Effect result = Effect.of_deferred_thunk (fun () -> Git.Queries.status ()) in
    let%bind.Effect current = Effect.of_thunk (fun () -> !generation) in
    if current = mine
    then set_load (Loaded (Or_error.map result ~f:(fun (_raw, entries, branch) -> entries, branch)))
    else Effect.Ignore
  in
  (* Only the FIRST load shows the loading message. *)
  let fetch =
    let%arr load and set_load and fetch_now in
    match load with
    | Load.Not_loaded -> Effect.Many [ set_load Loading; fetch_now ]
    | Loading | Loaded _ -> Effect.Ignore
  in
  Bonsai.Edge.before_display fetch graph;
  let refresh = fetch_now in
  (* Whichever pane acted last owns the selection. *)
  let active, set_active = Bonsai.state `Unstaged graph in
  let section ~which ~filter =
    let entries =
      let%arr load in
      List.filter (entries_of_load load) ~f:filter
    in
    let model, inject =
      Bonsai.state_machine_with_input
        ~default_model:Panes.Files.State.Model.initial
        ~apply_action:(fun _ctx input model action ->
          let entries =
            match input with
            | Bonsai.Computation_status.Active entries -> entries
            | Inactive -> []
          in
          Panes.Files.State.apply_action ~entries model action)
        entries
        graph
    in
    let rows =
      let%arr entries and model in
      Panes.Files.Tree.rows ~entries ~collapsed:model.collapsed
    in
    (* The action transition keeps the MODEL in range per action; this
       derivation keeps the EXPOSED cursor valid when the data changes (a
       refresh that shrinks the tree must not blank the pane). *)
    let cursor =
      let%arr rows and model in
      Int.clamp_exn model.cursor ~min:0 ~max:(Int.max 0 (List.length rows - 1))
    in
    let inject =
      let%arr inject and set_active in
      fun action -> Effect.Many [ set_active which; inject action ]
    in
    let selection =
      let%arr rows and cursor in
      Panes.Files.State.selection rows ~cursor
      |> Option.map ~f:(fun path -> path, which)
    in
    { rows; cursor; inject }, selection
  in
  let staged, staged_selection = section ~which:`Staged ~filter:Panes.Files.Sections.is_staged in
  let unstaged, unstaged_selection =
    section ~which:`Unstaged ~filter:Panes.Files.Sections.is_unstaged
  in
  let selection =
    let%arr active and staged_selection and unstaged_selection in
    match active with
    | `Staged -> staged_selection
    | `Unstaged -> unstaged_selection
  in
  let revision, bump_revision =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ctx revision () -> revision + 1)
      graph
  in
  let notice, set_notice = Bonsai.state None graph in
  (* An index mutation, then resync: the status reloads and [revision] tells
     the diff pane its content is stale. Failures surface in the status bar
     (first line only) and clear on the next success; the refresh runs
     either way — it IS the recovery. *)
  let mutate =
    let%arr refresh and bump_revision and set_notice in
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
      Effect.Many [ set_notice notice; bump_revision (); refresh ]
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
    let%arr set_load and bump_revision and peek_selection in
    let%bind.Effect mine = Effect.of_thunk (fun () -> !generation) in
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
        status, String.concat ~sep:"|" [ raw; index_m; selected_m ])
    in
    let status, signature = outcome in
    let%bind.Effect decision =
      Effect.of_thunk (fun () ->
        if !generation <> mine
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
      Effect.Many
        [ set_load
            (Loaded (Or_error.map status ~f:(fun (_raw, entries, branch) -> entries, branch)))
        ; bump_revision ()
        ]
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
  ; selection
  ; revision
  ; stage_path
  ; unstage_path
  ; stage_hunk
  ; unstage_hunk
  ; discard_path
  ; commit
  ; branch
  ; notice
  }
;;
