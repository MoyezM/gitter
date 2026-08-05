open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The app root: wires the layers together.

     Menu (Space overlay)
       -> Layout (panes)
            -> leaves
   ... vcat'd above the status bar. (A Mode screen-router slots in above
   Layout when a second screen exists.)

   Layout's state is created here so its control handle can feed the menu's
   command tree — the pattern every layer with commands will follow. *)

let layout_tree ~(data : Git_data.t) =
  let diff_title =
    let%arr selection = data.selection in
    Option.value selection ~default:"Diff"
  in
  Layout.Component.Tree.(
    split
      `Row
      [ ( 1.
        , leaf ~id:"files" ~title:(Bonsai.return "Files") (Leaves.Files.component ~data) )
      ; ( 2.
        , leaf
            ~id:"diff"
            ~title:diff_title
            (Leaves.Diff.component ~selection:data.selection) )
      ])
;;

let commands ~(layout : Layout.Component.Controls.t Bonsai.t) ~refresh =
  let%arr layout and refresh in
  [ Menu.Commands.Group
      { key = 'w'
      ; label = "window"
      ; children =
          [ Action { key = 'z'; label = "zoom"; effect = layout.toggle_zoom }
          ; Action { key = 'n'; label = "next pane"; effect = layout.focus_next }
          ]
      }
  ; Menu.Commands.Group
      { key = 'g'
      ; label = "git"
      ; children = [ Action { key = 'r'; label = "reload status"; effect = refresh } ]
      }
  ]
;;

let app ~(dimensions : Dimensions.t Bonsai.t) (local_ graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let screen_dimensions =
    let%arr dimensions in
    { Dimensions.height = Int.max 0 (dimensions.height - 1)
    ; width = dimensions.width
    }
  in
  let data = Git_data.create graph in
  let layout_tree = layout_tree ~data in
  let layout_model, layout_inject = Layout.Component.state layout_tree graph in
  let controls = Layout.Component.controls ~inject:layout_inject in
  let screen =
    Layout.Component.component layout_tree ~model:layout_model ~inject:layout_inject
  in
  let with_menu =
    Menu.Component.component
      ~commands:(commands ~layout:controls ~refresh:data.refresh)
      screen
  in
  let ~view:screen_view, ~handler = with_menu ~dimensions:screen_dimensions graph in
  let view =
    let%arr screen_view and dimensions in
    View.vcat
      [ screen_view
      ; Status_bar.render
          ~left:" gitter "
          ~right:"Space:menu  Tab:focus  Ctrl-C:quit "
          ~width:dimensions.width
      ]
  in
  ~view, ~handler
;;
