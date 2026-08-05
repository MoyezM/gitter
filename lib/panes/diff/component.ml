open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* Graph wiring only — everything with logic lives in the pure siblings
   (Document/State/Render) and the async edge (Fetch). *)

let component ~(selection : string option Bonsai.t) : Widget.t =
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
  let callback =
    let%arr set_result and inject in
    fun (selection : string option) ->
      match selection with
      | None -> Fetch.clear fetch ~set:set_result
      | Some path ->
        let%bind.Effect () = inject State.Action.Reset in
        Fetch.load fetch ~path ~set:set_result
  in
  Bonsai.Edge.on_change ~equal:[%equal: string option] ~callback selection graph;
  let view =
    let%arr content and model and dimensions in
    Render.render ~content ~model ~dimensions
  in
  let handler =
    let%arr doc_input and inject in
    let doc, _height = doc_input in
    fun (event : Event.t) ->
      if Array.is_empty doc
      then Effect.Ignore
      else (
        match event with
        | Event.Key_press { key = ASCII 'j'; mods = [] }
        | Key_press { key = Arrow `Down; mods = [] } -> inject (State.Action.Move 1)
        | Key_press { key = ASCII 'k'; mods = [] }
        | Key_press { key = Arrow `Up; mods = [] } -> inject (Move (-1))
        (* Half-page jumps. *)
        | Key_press { key = ASCII ('n' | '['); mods = [] } -> inject (Half_page 1)
        | Key_press { key = ASCII ('p' | ']'); mods = [] } -> inject (Half_page (-1))
        | Mouse { kind = Scroll `Down; _ } -> inject (Wheel 1)
        | Mouse { kind = Scroll `Up; _ } -> inject (Wheel (-1))
        | Mouse { kind = Left; position; _ } -> inject (Click position.y)
        | _ -> Effect.Ignore)
  in
  ~view, ~handler
;;
