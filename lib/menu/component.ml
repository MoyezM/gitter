open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The Space-menu combinator: wraps a widget; Space opens a helix-style
   which-key popup driven by a declarative [Commands.t] tree, and [?] within
   it opens the command palette over the same commands, flattened.

   Event precedence: while the popup is open, a mnemonic descends or fires
   (then closes), [?] opens the palette, Escape/Space/unmatched/mouse
   closes. In the palette, printable keys type, arrows move, Enter runs.
   While closed, only Space is intercepted. *)

let overlay ~(model : State.Model.t) ~commands ~dimensions ~base =
  match model with
  | Closed -> base
  | Menu path ->
    (match Commands.resolve commands path with
     | None | Some (_, []) -> base
     | Some (title, entries) ->
       View.zcat [ Render.render ~entries ~title ~dimensions; base ])
  | Palette { query; cursor } ->
    let flats = Commands.flatten commands in
    let filtered = Palette.filter ~query flats in
    View.zcat
      [ Palette.render ~filtered ~total:(List.length flats) ~query ~cursor ~dimensions
      ; base
      ]
;;

let handle_menu ~inject ~commands ~path (event : Event.t) =
  match event with
  | Event.Key_press { key = ASCII '?'; mods = [] } -> inject State.Action.Open_palette
  | Key_press { key = Escape; _ } | Key_press { key = ASCII ' '; mods = [] } ->
    inject State.Action.Close
  | Key_press { key = ASCII c; mods = [] } ->
    let entry =
      Option.bind (Commands.resolve commands path) ~f:(fun (_title, entries) ->
        Commands.find entries c)
    in
    (match entry with
     | Some (Commands.Action { effect; _ }) ->
       Effect.Many [ inject State.Action.Close; effect ]
     | Some (Group _) -> inject (Descend c)
     | None -> inject Close)
  | Key_press _ | Mouse _ -> inject State.Action.Close
  | Paste _ -> Effect.Ignore
;;

let handle_palette ~inject ~commands ~query ~cursor (event : Event.t) =
  (* Deferred: only the Enter and Arrow-Down arms need the filtered list. *)
  let filtered () = Palette.filter ~query (Commands.flatten commands) in
  match event with
  | Event.Key_press { key = Escape; _ } -> inject State.Action.Close
  | Key_press { key = Enter; _ } ->
    (match List.nth (filtered ()) cursor with
     | Some (f : Commands.Flat.t) -> Effect.Many [ inject State.Action.Close; f.effect ]
     | None -> inject Close)
  | Key_press { key = Backspace; _ } -> inject Query_backspace
  | Key_press { key = Arrow `Up; _ } -> inject (Set_cursor (Int.max 0 (cursor - 1)))
  | Key_press { key = Arrow `Down; _ } ->
    inject (Set_cursor (Int.max 0 (Int.min (List.length (filtered ()) - 1) (cursor + 1))))
  | Key_press { key = ASCII c; mods = [] } -> inject (Query_append c)
  | Key_press _ | Paste _ | Mouse _ -> Effect.Ignore
;;

let component
  ~(commands : Commands.t list Bonsai.t)
  (base : Widget.screen)
  ~dimensions
  (local_ graph)
  : Widget.screen
  =
  let model, inject =
    Bonsai.state_machine
      ~default_model:State.Model.Closed
      ~apply_action:State.apply_action
      graph
  in
  let view =
    let%arr base_view = base.Widget.view
    and model
    and commands
    and dimensions in
    overlay ~model ~commands ~dimensions ~base:base_view
  in
  let handler =
    let%arr base_handler = base.Widget.handler
    and inject
    and model
    and commands
    and search_active = base.Widget.search_active in
    fun (event : Event.t) ->
      match model with
      | State.Model.Menu path -> handle_menu ~inject ~commands ~path event
      | Palette { query; cursor } -> handle_palette ~inject ~commands ~query ~cursor event
      | Closed ->
        (match event with
         (* While a pane's search prompt is active, Space belongs to the
            query — nothing printable is stolen (keys table); the pane
            below appends it. The screen carries that fact here — no
            side channel. *)
         | Event.Key_press { key = ASCII ' '; mods = [] } when not search_active ->
           inject State.Action.Open_menu
         | event -> base_handler event)
  in
  let hints =
    (* Space is the MENU's binding — appended here once, not restated
       by every leaf. *)
    let%arr base_hints = base.Widget.hints in
    base_hints ^ "  Space:menu"
  in
  { base with Widget.view; handler; hints }
;;
