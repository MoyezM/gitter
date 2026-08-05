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
  let model, inject =
    Bonsai.state_machine_with_input
      ~default_model:State.Model.initial
      ~apply_action:(fun _ctx input model action ->
        match input with
        | Bonsai.Computation_status.Inactive -> model
        | Active (doc, height) -> State.apply_action doc model action ~height)
      doc_input
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
            not ([%equal: (string * [ `Staged | `Unstaged ]) option option]
                   (Some selection)
                   !last_key)
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
    ~equal:[%equal: (string * [ `Staged | `Unstaged ]) option * int]
    ~callback
    watch
    graph;
  let view =
    let%arr content and model and dimensions in
    Render.render ~content ~model ~dimensions
  in
  let handler =
    let%arr doc_input and inject and model and result and selection and stage_hunk and unstage_hunk in
    let doc, height = doc_input in
    (* s/u apply the hunk enclosing the (visible) cursor to the index; the
       raw hunk bytes come from the payload's parsed files. Failure (index
       moved underneath) resyncs via the mutation's refresh — silent. *)
    let hunk_op ~wanted_side op =
      match selection, result with
      | Some ((path, side) as key), Some (rkey, Ok (payload : Fetch.payload))
        when [%equal: string * [ `Staged | `Unstaged ]] key rkey
             && [%equal: [ `Staged | `Unstaged ]] side wanted_side ->
        let cursor = State.effective_cursor doc model ~height in
        (match Document.hunk_at doc ~row:cursor with
         | None -> Effect.Ignore
         | Some (fi, hi) ->
           (match
              Option.bind (List.nth payload.files fi) ~f:(fun f -> List.nth f.hunks hi)
            with
            | Some hunk -> op ~path ~raw:hunk.Git.Diff.Hunk.raw
            | None -> Effect.Ignore))
      | _ -> Effect.Ignore
    in
    fun (event : Event.t) ->
      if Array.is_empty doc
      then Effect.Ignore
      else (
        match event with
        | Event.Key_press { key = ASCII 's'; mods = [] } ->
          hunk_op ~wanted_side:`Unstaged stage_hunk
        | Key_press { key = ASCII 'u'; mods = [] } ->
          hunk_op ~wanted_side:`Staged unstage_hunk
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
        | Mouse { kind = Left; position; _ } -> inject (Click position.y)
        | _ -> Effect.Ignore)
  in
  ~view, ~handler
;;
