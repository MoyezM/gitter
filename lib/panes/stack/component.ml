open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The branch-stack pane: a foldable tree with a cursor. The state machine
   lives here (unlike the files panes, nothing outside consumes the
   selection yet — set-as-base arrives with relative mode). All mutations
   go through the machine; the wheel scrolls without moving the cursor.
   Search integration is [Search.Action] (keymap, mode dispatch,
   lifecycle); the layout reads the search surface off the returned
   [Widget.leaf]. *)

let component ~status ~base ~set_base : Widget.leaf =
  fun ~dimensions (local_ graph) ->
  let branches =
    let%arr status in
    match status with
    | `Stack branches -> branches
    | `Loading | `Error _ | `Empty _ -> []
  in
  let machine_input =
    let%arr branches and set_base in
    branches, set_base
  in
  let model, inject =
    Bonsai.state_machine_with_input
      ~default_model:State.Model.initial
      ~apply_action:(fun ctx input model action ->
        let branches, set_base =
          match input with
          | Bonsai.Computation_status.Active input -> input
          | Inactive -> [], fun (_ : string) -> Effect.Ignore
        in
        (* Mode-dependent keys resolve against the CURRENT model first,
           so the Enter-as-set-base reading below cannot fire off a
           stale handler snapshot while the prompt is active; the
           transition resolves identically (idempotent). *)
        match Search.Action.resolve ~search:model.Search.Tree_search.Model.search action with
        | None -> model
        | Some action ->
          (match action with
           | Search.Action.Pane (State.Action.Enter _) ->
             (* Base-setting resolves against the CURRENT selection. *)
             (* Resolve BY KEY: [index_of] defaults a missing/unset
                selection to row 0, so an Enter with no selection (initial
                model) or a stale key would silently swap the base to row
                0's branch — the same guard as [Files.State.target]. *)
             let rows = State.visible_rows ~branches model in
             let selected =
               match State.selection_key model with
               | None -> None
               | Some key ->
                 List.find rows ~f:(fun (r : State.Row.t) -> String.equal r.key key)
             in
             (match selected with
              | Some { State.Row.kind = Branch b; _ } when not b.is_current ->
                Bonsai.Apply_action_context.schedule_event ctx (set_base b.name)
              | Some _ | None -> ())
           | _ -> ());
          State.apply_action ~branches model action)
      machine_input
      graph
  in
  (* ONE derivation of the displayed rows — shared by the view, the
     handler and the repair edge — built from field projections, which
     are phys-stable across listing-only updates: the forest neither
     rebuilds once per consumer nor once per wheel tick. *)
  let overrides =
    let%arr model in
    model.Search.Tree_search.Model.fold
  in
  let search =
    let%arr model in
    model.Search.Tree_search.Model.search
  in
  let rows =
    let%arr branches and overrides and search in
    State.displayed_rows ~branches ~overrides ~search
  in
  (* Selection is a key; repair it whenever the visible rows' keys change
     (poller refreshes, recency resorts, fold toggles, filter churn). *)
  Bonsai.Edge.on_change
    ~equal:[%equal: string list]
    ~callback:
      (let%arr inject in
       fun (_ : string list) -> inject (Search.Action.pane (State.Action.Nav Tree_listing.Action.Rows_changed)))
    (let%arr rows in
     List.map rows ~f:(fun (r : State.Row.t) -> r.key))
    graph;
  let view =
    let%arr status and rows and model and base and dimensions in
    Render.render ~status ~rows ~model ~base ~dimensions
  in
  let border =
    let%arr branches and search and dimensions in
    State.border ~branches search ~width:dimensions.width
  in
  let handler =
    let%arr inject and rows and model and dimensions in
    let height = dimensions.height in
    fun (event : Event.t) ->
      let pane = Search.Action.pane in
      let lift a = State.Action.Nav a in
      (* Enter idle sets the committed view's base — deliberately
         Enter-ONLY: clicks navigate and fold, and a misclick must not
         silently swap what the Committed pane diffs against. *)
      match
        Search.Action.keymap
          ~height
          ~idle_char:(Tree_listing.idle_char ~height ~lift)
          ~enter_idle:(pane (State.Action.Enter { height }))
          event
      with
      | Some action -> inject action
      | None ->
        Tree_listing.handle
          ~height
          ~total:(List.length rows)
          ~scroll:(State.scroll model)
          ~inject
          ~lift
          event
  in
  let search_active, commit_search = Search.Action.surface ~inject ~search in
  { Widget.view
  ; border
  ; search_active
  ; commit_search
  ; handler
  ; hints = "j/k:move  h/l:fold  /:search  Enter:set base"
  }
;;
