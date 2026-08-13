open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* Graph wiring only — everything with logic lives in the pure siblings
   (Document/State/Render) and the async edge (Fetch). *)

let component
      ~(selection : Fetch.key option Bonsai.t)
      ~(revision : int Bonsai.t)
      ~stage_hunk
      ~unstage_hunk
      ~copy_path
  : Widget.leaf
  =
  fun ~dimensions (local_ graph) ->
  let result, set_result = Bonsai.state None graph in
  let fetch = Fetch.create () in
  let content =
    let%arr selection and result in
    Render.content ~selection ~result
  in
  let doc_input =
    let%arr content and dimensions in
    let source =
      match content with
      | `Message _ -> Document.Source.empty
      | `Document (source, _, _) -> source
    in
    source, dimensions.height
  in
  let machine_input =
    let%arr doc_input and selection and result and stage_hunk and unstage_hunk and copy_path in
    doc_input, selection, result, (stage_hunk, unstage_hunk, copy_path)
  in
  let model, inject =
    Bonsai.state_machine_with_input
      ~default_model:State.Model.initial
      ~apply_action:(fun ctx input model action ->
        match input with
        | Bonsai.Computation_status.Inactive -> model
        | Active ((source, height), selection, result, (stage_hunk, unstage_hunk, copy_path))
          ->
          (* Everything resolves against the CURRENT model — the key's
             mode-dependent reading (a stale handler snapshot cannot
             stage a hunk out of a half-typed query) and the Operate
             targets (a same-frame [j, s] burst stages the hunk j moved
             to); the transition resolves identically (idempotent). *)
          (match Search.Action.resolve ~search:model.State.Model.search action with
           | None -> model
           | Some (Search.Action.Pane (State.Action.Operate op)) ->
             let schedule e = Bonsai.Apply_action_context.schedule_event ctx e in
             (match op, selection with
              | `Copy_line, Some (path, _) ->
                schedule (copy_path (State.yank_target (State.shown source model) model ~path))
              | (`Stage_hunk | `Unstage_hunk), Some ((path, side) as key) ->
                let wanted, run =
                  match op with
                  | `Stage_hunk -> `Unstaged, stage_hunk
                  | `Unstage_hunk -> `Staged, unstage_hunk
                  | `Copy_line -> assert false
                in
                (match result with
                 | Some (rkey, Ok (_ : Fetch.payload))
                   when [%equal: Fetch.key] key rkey
                        && [%equal: Fetch.side] side (wanted :> Fetch.side) ->
                   (* The hunk is resolved from the cursor's LINE NUMBER
                      against the parse — display-independent, so no mask
                      change can retarget it. *)
                   let doc = State.shown source model in
                   (match
                      Document.hunk_under
                        source
                        doc
                        ~row:(State.effective_cursor doc model)
                    with
                    | Some hunk -> schedule (run ~path ~raw:hunk.Git.Diff.Hunk.raw)
                    | None -> ())
                 | _ -> ())
              | _, None -> ());
             model
           | Some action -> State.apply_action source model action ~height))
      machine_input
      graph
  in
  (* Refetch when the selection changes AND when the revision bumps: an
     index mutation leaves the same file selected with different content. *)
  let watch =
    let%arr selection and revision in
    selection, revision
  in
  (* The viewport resets only when the SELECTION changed — a revision bump
     (stage/unstage resync of the same file) keeps cursor and scroll, so the
     view doesn't jump to the top on every staging action. *)
  let last_key : Fetch.key option option ref = ref None in
  let callback =
    let%arr set_result and inject in
    fun ((selection : Fetch.key option), (_revision : int)) ->
      let%bind.Effect key_changed =
        Effect.of_thunk (fun () ->
          let changed =
            not ([%equal: Fetch.key option option] (Some selection) !last_key)
          in
          last_key := Some selection;
          changed)
      in
      match selection with
      | None -> Fetch.clear fetch ~set:set_result
      | Some key ->
        let%bind.Effect () = if key_changed then inject (Search.Action.pane State.Action.Reset) else Effect.Ignore in
        Fetch.load fetch ~key ~set:set_result
  in
  Bonsai.Edge.on_change
    ~equal:[%equal: Fetch.key option * int]
    ~callback
    watch
    graph;
  (* When the document is REPLACED under the kept cursor (a revision-bump
     refetch after staging), re-anchor with [Reveal] so hunk staging can't
     silently retarget. [Fetch.doc_id] is that signal as a value: stable
     across one load's highlight phases (which must not yank the
     viewport), distinct per load. *)
  Bonsai.Edge.on_change
    ~equal:[%equal: int option]
    ~callback:
      (let%arr inject in
       fun (_ : int option) -> inject (Search.Action.pane State.Action.Reveal))
    (let%arr result in
     match result with
     | Some (_, Ok payload) -> Some payload.Fetch.doc_id
     | _ -> None)
    graph;
  let search_active, commit_search =
    Search.Action.surface
      ~inject
      ~search:
        (let%arr model in
         model.State.Model.search)
  in
  let view =
    let%arr content and model and dimensions in
    Render.render ~content ~model ~dimensions
  in
  (* The search prompt/register in the pane's bottom border, with the
     [total · hidden] / [current/total · hidden] counter (D6, R5). *)
  let border =
    let%arr doc_input and model and dimensions in
    let source, (_ : int) = doc_input in
    Search.Border.view
      ~prompt:model.State.Model.search
      ~counts:(State.search_counts source model)
      ~width:dimensions.width
  in
  let handler =
    let%arr doc_input and inject and model in
    let source, height = doc_input in
    let doc = State.shown source model in
    let pane = Search.Action.pane in
    (* Only this pane's own bindings live here — the keymap, mode
       dispatch and lifecycle are [Search.Action]'s. Typing moves
       nothing (D3); the arrows jump between matches without leaving
       the prompt. Search is total on degraded documents (D8): the
       prompt opens — and a faded register clears — even over an empty
       one; every transition is total there too. *)
    let idle_char c =
      match c with
      (* Effectful keys go through the machine — targets resolve at
         APPLY time. y works even with no document (degrades to the
         path). *)
      | 's' -> Some (pane (State.Action.Operate `Stage_hunk))
      | 'u' -> Some (pane (State.Action.Operate `Unstage_hunk))
      | 'y' -> Some (pane (State.Action.Operate `Copy_line))
      | 'j' -> Some (pane (State.Action.Move 1))
      | 'k' -> Some (pane (State.Action.Move (-1)))
      (* Context: K pulls more in above the reading position, J below,
         X folds the boundary at the cursor. notty reports shifted
         letters as plain uppercase with no modifier. *)
      | 'K' -> Some (pane (State.Action.Context `Up))
      | 'J' -> Some (pane (State.Action.Context `Down))
      | 'X' -> Some (pane (State.Action.Context `Reset))
      (* Bracket aliases for the half-page jumps. *)
      | '[' -> Some (pane (State.Action.Half_page 1))
      | ']' -> Some (pane (State.Action.Half_page (-1)))
      (* Horizontal pan of the content area (gutter stays put). *)
      | 'l' -> Some (pane (State.Action.Pan 1))
      | 'h' -> Some (pane (State.Action.Pan (-1)))
      | _ -> None
    in
    fun (event : Event.t) ->
      match Search.Action.keymap ~height ~idle_char event with
      | Some action -> inject action
      | None ->
        (match event with
         (* Arrows: match jumps while the prompt is active (D3), ordinary
            motions otherwise. *)
         | Event.Key_press { key = Arrow `Down; mods = [] } ->
           inject
             (Search.Action.both
                ~active:(Search.Action.jump ~height 1)
                ~idle:(pane (State.Action.Move 1)))
         | Key_press { key = Arrow `Up; mods = [] } ->
           inject
             (Search.Action.both
                ~active:(Search.Action.jump ~height (-1))
                ~idle:(pane (State.Action.Move (-1))))
         | Key_press { key = Arrow `Right; mods = [] } ->
           inject (Search.Action.when_idle (pane (State.Action.Pan 1)))
         | Key_press { key = Arrow `Left; mods = [] } ->
           inject (Search.Action.when_idle (pane (State.Action.Pan (-1))))
         (* Half-page jumps: the helix keys. *)
         | _ when Chord.ctrl 'd' event -> inject (pane (State.Action.Half_page 1))
         | _ when Chord.ctrl 'u' event -> inject (pane (State.Action.Half_page (-1)))
         | Mouse { kind = Scroll `Down; _ } -> inject (pane (State.Action.Wheel 1))
         | Mouse { kind = Scroll `Up; _ } -> inject (pane (State.Action.Wheel (-1)))
         | Mouse { kind = Left; position; _ } ->
           (* Map through the scroll this handler PAINTED with (render
              clamps identically), so the click hits what the user saw —
              then commit any active prompt (keys table). *)
           let click =
             if Array.is_empty doc
             then []
             else
               [ inject
                   (pane
                      (State.Action.Click
                         { row = State.clamp_scroll doc ~height model.scroll + position.y
                         ; column = position.x
                         }))
               ]
           in
           Effect.Many (click @ [ inject Search.Action.implicit_commit ])
         | _ -> Effect.Ignore)
  in
  { Widget.view
  ; border
  ; search_active
  ; commit_search
  ; handler
  ; hints = "j/k:move  C-d/C-u:page  /:search  n/N:match  s/u:\u{00B1}hunk  y:copy"
  }
;;
