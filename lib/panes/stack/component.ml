open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The branch-stack pane: a foldable tree with a cursor. The state machine
   lives here (unlike the files panes, nothing outside consumes the
   selection yet — set-as-base arrives with relative mode). All mutations
   go through the machine; the wheel scrolls without moving the cursor. *)

let component ~status ~base ~set_base : Widget.t =
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
        (match action with
         | State.Action.Enter _ ->
           (* Base-setting resolves against the CURRENT selection. *)
           let rows = State.visible ~branches ~overrides:model.overrides in
           (match List.nth rows (State.index_of rows (State.selection_key model)) with
            | Some { State.Row.kind = Branch b; _ } when not b.is_current ->
              Bonsai.Apply_action_context.schedule_event ctx (set_base b.name)
            | Some _ | None -> ())
         | _ -> ());
        State.apply_action ~branches model action)
      machine_input
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
    let%arr status and model and base and dimensions in
    Render.render ~status ~model ~base ~dimensions
  in
  let handler =
    let%arr inject and branches and model and dimensions in
    let height = dimensions.height in
    let rows = State.visible ~branches ~overrides:model.overrides in
    let offset = Listing.offset ~total:(List.length rows) ~height (State.scroll model) in
    fun (event : Event.t) ->
      match event with
      (* Enter sets the committed view's base — deliberately Enter-ONLY:
         clicks navigate and fold, and a misclick must not silently swap
         what the Committed pane diffs against. Routed through the machine
         so the target resolves at APPLY time. *)
      | Event.Key_press { key = Enter; mods = [] } ->
        inject (State.Action.Enter { height })
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
