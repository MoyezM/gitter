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
  : Widget.t
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
    let doc =
      match content with
      | `Message _ -> [||]
      | `Document (doc, _, _) -> doc
    in
    doc, dimensions.height
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
        | Active ((doc, height), selection, result, (stage_hunk, unstage_hunk, copy_path))
          ->
          (match action with
           | State.Action.Operate op ->
             (* Effectful keys resolve against the CURRENT model — a
                same-frame [j, s] burst stages the hunk j moved to. *)
             let schedule e = Bonsai.Apply_action_context.schedule_event ctx e in
             (match op, selection with
              | `Copy_line, Some (path, _) ->
                schedule (copy_path (State.yank_target doc model ~path))
              | (`Stage_hunk | `Unstage_hunk), Some ((path, side) as key) ->
                let wanted, run =
                  match op with
                  | `Stage_hunk -> `Unstaged, stage_hunk
                  | `Unstage_hunk -> `Staged, unstage_hunk
                  | `Copy_line -> assert false
                in
                (match result with
                 | Some (rkey, Ok (payload : Fetch.payload))
                   when [%equal: Fetch.key] key rkey
                        && [%equal: Fetch.side] side (wanted :> Fetch.side) ->
                   let cursor = State.effective_cursor doc model in
                   (match Document.hunk_at doc ~row:cursor with
                    | None -> ()
                    | Some (fi, hi) ->
                      (match
                         Option.bind (List.nth payload.files fi) ~f:(fun f ->
                           List.nth f.hunks hi)
                       with
                       | Some hunk -> schedule (run ~path ~raw:hunk.Git.Diff.Hunk.raw)
                       | None -> ()))
                 | _ -> ())
              | _, None -> ());
             model
           | action -> State.apply_action doc model action ~height))
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
        let%bind.Effect () = if key_changed then inject State.Action.Reset else Effect.Ignore in
        Fetch.load fetch ~key ~set:set_result
  in
  Bonsai.Edge.on_change
    ~equal:[%equal: Fetch.key option * int]
    ~callback
    watch
    graph;
  (* When the document's SHAPE changes under the kept cursor (a
     revision-bump refetch after staging shrinks/renumbers it), re-anchor
     with [Reveal] so hunk staging can't silently retarget. Keyed on the
     row count, not the payload: the two-phase highlight swap-in delivers
     an identical document and must not yank the viewport. *)
  Bonsai.Edge.on_change
    ~equal:Int.equal
    ~callback:
      (let%arr inject in
       fun (_ : int) -> inject State.Action.Reveal)
    (let%arr doc_input in
     Array.length (fst doc_input))
    graph;
  let view =
    let%arr content and model and dimensions in
    Render.render ~content ~model ~dimensions
  in
  let handler =
    let%arr doc_input and inject and model in
    let doc, height = doc_input in
    fun (event : Event.t) ->
      match event with
      (* Effectful keys go through the machine — targets resolve at APPLY
         time. y works even with no document (degrades to the path). *)
      | Event.Key_press { key = ASCII 'y'; mods = [] } ->
        inject (State.Action.Operate `Copy_line)
      | event ->
        if Array.is_empty doc
        then Effect.Ignore
        else (
          match event with
          | Event.Key_press { key = ASCII 's'; mods = [] } ->
            inject (State.Action.Operate `Stage_hunk)
          | Key_press { key = ASCII 'u'; mods = [] } ->
            inject (State.Action.Operate `Unstage_hunk)
          | Event.Key_press { key = ASCII 'j'; mods = [] }
        | Key_press { key = Arrow `Down; mods = [] } -> inject (State.Action.Move 1)
        | Key_press { key = ASCII 'k'; mods = [] }
        | Key_press { key = Arrow `Up; mods = [] } -> inject (Move (-1))
        (* Half-page jumps. *)
        | Key_press { key = ASCII ('n' | '['); mods = [] } -> inject (Half_page 1)
        | Key_press { key = ASCII ('p' | ']'); mods = [] } -> inject (Half_page (-1))
        (* Horizontal pan of the content area (gutter stays put). *)
        | Key_press { key = ASCII 'l'; mods = [] }
        | Key_press { key = Arrow `Right; mods = [] } -> inject (Pan 1)
        | Key_press { key = ASCII 'h'; mods = [] }
        | Key_press { key = Arrow `Left; mods = [] } -> inject (Pan (-1))
        | Mouse { kind = Scroll `Down; _ } -> inject (Wheel 1)
        | Mouse { kind = Scroll `Up; _ } -> inject (Wheel (-1))
        | Mouse { kind = Left; position; _ } ->
          (* Map through the scroll this handler PAINTED with (render
             clamps identically), so the click hits what the user saw. *)
          inject (Click (State.clamp_scroll doc ~height model.scroll + position.y))
        | _ -> Effect.Ignore)
  in
  ~view, ~handler
;;
