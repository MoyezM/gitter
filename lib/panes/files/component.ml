open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* Presentation plus event forwarding for ONE section's tree; the state
   lives in [Git_data] (the root owns it because the derived selection
   feeds the diff pane). Two instances — Staged and Changes — sit in their
   own layout panes, which is what makes them independently scrollable,
   focusable, and resizable. *)

let component ~status ~rows ~cursor ~scroll ~counts ~reviewed ~side ~commit ~inject
  : Widget.t
  =
  fun ~dimensions (local_ _graph) ->
  let view =
    let%arr status and rows and cursor and scroll and counts and reviewed and dimensions in
    Render.render ~status ~rows ~cursor ~scroll ~counts ~reviewed ~side ~dimensions
  in
  let handler =
    let%arr inject and scroll and rows and commit and dimensions in
    fun (event : Event.t) ->
      let height = dimensions.height in
      let offset = Listing.offset ~total:(List.length rows) ~height scroll in
      match event with
      (* Effectful keys go through the machine: the target resolves at
         APPLY time, so same-frame bursts act on the row the cursor is
         actually on. *)
      | Event.Key_press { key = ASCII 's'; mods = [] } -> inject (State.Action.Operate Stage)
      | Key_press { key = ASCII 'u'; mods = [] } -> inject (Operate Unstage)
      | Key_press { key = ASCII 'd'; mods = [] } -> inject (Operate Discard)
      | Key_press { key = ASCII 'c'; mods = [] } -> commit
      | Key_press { key = ASCII 'y'; mods = [] } -> inject (Operate Copy_path)
      | Key_press { key = ASCII 'r'; mods = [] } -> inject (Operate Toggle_review)
      | Key_press { key = ASCII 'j'; mods = [] }
      | Key_press { key = Arrow `Down; mods = [] } ->
        inject (State.Action.Move { dir = `Down; height })
      | Key_press { key = ASCII 'k'; mods = [] }
      | Key_press { key = Arrow `Up; mods = [] } -> inject (Move { dir = `Up; height })
      | Key_press { key = ASCII 'h'; mods = [] }
      | Key_press { key = Arrow `Left; mods = [] } -> inject (Collapse { height })
      | Key_press { key = ASCII 'l'; mods = [] }
      | Key_press { key = Arrow `Right; mods = [] } -> inject Expand
      | Mouse { kind = Scroll `Down; _ } -> inject (Wheel { dir = 1; height })
      | Mouse { kind = Scroll `Up; _ } -> inject (Wheel { dir = -1; height })
      | Mouse { kind = Left; position; _ } ->
        (* Blank space below a short tree is not a row — activating there
           would clamp onto the last row and toggle/select it. *)
        let row = position.y + offset in
        if row >= List.length rows
        then Effect.Ignore
        else inject (Activate { row; height })
      | _ -> Effect.Ignore
  in
  ~view, ~handler
;;
