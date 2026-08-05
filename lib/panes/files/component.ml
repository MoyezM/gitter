open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* Presentation plus event forwarding for ONE section's tree; the state
   lives in [Git_data] (the root owns it because the derived selection
   feeds the diff pane). Two instances — Staged and Changes — sit in their
   own layout panes, which is what makes them independently scrollable,
   focusable, and resizable. *)

let component
      ~status
      ~rows
      ~cursor
      ~scroll
      ~side
      ~stage
      ~unstage
      ~discard
      ~commit
      ~copy_path
      ~inject
  : Widget.t
  =
  fun ~dimensions (local_ _graph) ->
  let view =
    let%arr status and rows and cursor and scroll and dimensions in
    Render.render ~status ~rows ~cursor ~scroll ~side ~dimensions
  in
  let handler =
    let%arr inject
    and cursor
    and scroll
    and rows
    and stage
    and unstage
    and discard
    and commit
    and copy_path
    and dimensions in
    (* s/u act on the row under the cursor: a file's path, or a directory's
       whole subtree (git pathspecs make that the same operation). *)
    let operate op =
      match List.nth rows cursor with
      | Some (Tree.Dir { path; _ }) -> op path
      | Some (File { entry; _ }) -> op entry.Git.Status.Entry.path
      | None -> Effect.Ignore
    in
    fun (event : Event.t) ->
      let offset =
        State.offset ~total:(List.length rows) ~cursor ~height:dimensions.height scroll
      in
      match event with
      | Event.Key_press { key = ASCII 's'; mods = [] } -> operate stage
      | Key_press { key = ASCII 'u'; mods = [] } -> operate unstage
      | Key_press { key = ASCII 'd'; mods = [] } -> operate discard
      | Key_press { key = ASCII 'c'; mods = [] } -> commit
      | Key_press { key = ASCII 'y'; mods = [] } -> operate copy_path
      | Key_press { key = ASCII 'j'; mods = [] }
      | Key_press { key = Arrow `Down; mods = [] } -> inject (State.Action.Move `Down)
      | Key_press { key = ASCII 'k'; mods = [] }
      | Key_press { key = Arrow `Up; mods = [] } -> inject (Move `Up)
      | Key_press { key = ASCII 'h'; mods = [] }
      | Key_press { key = Arrow `Left; mods = [] } -> inject Collapse
      | Key_press { key = ASCII 'l'; mods = [] }
      | Key_press { key = Arrow `Right; mods = [] } -> inject Expand
      | Mouse { kind = Scroll `Down; _ } ->
        inject (Wheel { dir = 1; height = dimensions.height })
      | Mouse { kind = Scroll `Up; _ } ->
        inject (Wheel { dir = -1; height = dimensions.height })
      | Mouse { kind = Left; position; _ } -> inject (Activate (position.y + offset))
      | _ -> Effect.Ignore
  in
  ~view, ~handler
;;
