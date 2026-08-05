open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The branch-stack pane: a foldable tree with a cursor. The state machine
   lives here (unlike the files panes, nothing outside consumes the
   selection yet — set-as-base arrives with relative mode). All mutations
   go through the machine; the wheel scrolls without moving the cursor. *)

let component ~status : Widget.t =
  fun ~dimensions (local_ graph) ->
  let branches =
    let%arr status in
    match status with
    | `Stack branches -> branches
    | `Loading | `Error _ | `Empty _ -> []
  in
  let model, inject =
    Bonsai.state_machine_with_input
      ~default_model:State.Model.initial
      ~apply_action:(fun _ctx input model action ->
        let branches =
          match input with
          | Bonsai.Computation_status.Active branches -> branches
          | Inactive -> []
        in
        State.apply_action ~branches model action)
      branches
      graph
  in
  (* Selection is a key; repair it whenever the visible rows' keys change
     (poller refreshes, recency resorts, fold toggles). *)
  Bonsai.Edge.on_change
    ~equal:[%equal: string list]
    ~callback:
      (let%arr inject in
       fun (_ : string list) -> inject State.Action.Rows_changed)
    (let%arr branches and model in
     State.visible ~branches ~overrides:model.overrides
     |> List.map ~f:(fun (r : State.Row.t) -> r.key))
    graph;
  let view =
    let%arr status and model and dimensions in
    Render.render ~status ~model ~dimensions
  in
  let handler =
    let%arr inject and branches and model and dimensions in
    let height = dimensions.height in
    let rows = State.visible ~branches ~overrides:model.overrides in
    let offset = Listing.offset ~total:(List.length rows) ~height (State.scroll model) in
    fun (event : Event.t) ->
      match event with
      | Event.Key_press { key = ASCII 'j'; mods = [] }
      | Key_press { key = Arrow `Down; mods = [] } ->
        inject (State.Action.Move { dir = `Down; height })
      | Key_press { key = ASCII 'k'; mods = [] }
      | Key_press { key = Arrow `Up; mods = [] } -> inject (Move { dir = `Up; height })
      | Key_press { key = ASCII 'h'; mods = [] }
      | Key_press { key = Arrow `Left; mods = [] } -> inject (Fold { height })
      | Key_press { key = ASCII 'l'; mods = [] }
      | Key_press { key = Arrow `Right; mods = [] } -> inject (Unfold { height })
      | Mouse { kind = Scroll `Down; _ } -> inject (Wheel { dir = 1; height })
      | Mouse { kind = Scroll `Up; _ } -> inject (Wheel { dir = -1; height })
      | Mouse { kind = Left; position; _ } ->
        let row = position.y + offset in
        if row >= List.length rows
        then Effect.Ignore
        else inject (Activate { row; height })
      | _ -> Effect.Ignore
  in
  ~view, ~handler
;;
