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

let layout_tree ~(data : Git_data.t) ~discard ~commit =
  let diff_title =
    let%arr selection = data.selection in
    match selection with
    | Some (path, `Staged) -> path ^ " (staged)"
    | Some (path, `Unstaged) -> path
    | None -> "Diff"
  in
  let files_status rows ~empty =
    let%arr load = data.load
    and rows in
    match load with
    | Git_data.Load.Not_loaded | Loading -> `Loading
    | Loaded (Error e) -> `Error e
    | Loaded (Ok _) -> if List.is_empty rows then `Empty empty else `Tree
  in
  let noop = Bonsai.return (fun (_ : string) -> Effect.Ignore) in
  let files_pane ~id ~title ~side ~stage ~unstage ~discard (section : Git_data.section_data) ~empty
    =
    Layout.Component.Tree.leaf
      ~id
      ~title:(Bonsai.return title)
      (Panes.Files.Component.component
         ~status:(files_status section.rows ~empty)
         ~rows:section.rows
         ~cursor:section.cursor
         ~side
         ~stage
         ~unstage
         ~discard
         ~commit
         ~inject:section.inject)
  in
  Layout.Component.Tree.(
    split
      `Row
      [ ( 1.
        , split
            `Col
            [ ( 1.
              , files_pane
                  ~id:"staged"
                  ~title:"Staged"
                  ~side:`Staged
                  ~stage:noop
                  ~unstage:data.unstage_path
                  ~discard:noop (* staged entries don't discard *)
                  data.staged
                  ~empty:"nothing staged" )
            ; ( 2.
              , files_pane
                  ~id:"changes"
                  ~title:"Changes"
                  ~side:`Unstaged
                  ~stage:data.stage_path
                  ~unstage:noop
                  ~discard
                  data.unstaged
                  ~empty:"working tree clean" )
            ] )
      ; ( 2.
        , leaf
            ~id:"diff"
            ~title:diff_title
            (Panes.Diff.Component.component
               ~selection:data.selection
               ~revision:data.revision
               ~stage_hunk:data.stage_hunk
               ~unstage_hunk:data.unstage_hunk) )
      ])
;;

let commands ~(layout : Layout.Component.Controls.t Bonsai.t) ~refresh ~commit =
  let%arr layout and refresh and commit in
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
      ; children =
          [ Action { key = 'r'; label = "reload status"; effect = refresh }
          ; Action { key = 'c'; label = "commit"; effect = commit }
          ]
      }
  ]
;;

(* Context hints for the status bar: the focused pane's keys. *)
let hints ~focused =
  match focused with
  | "staged" -> "j/k:move  h/l:fold  u:unstage  c:commit  Space:menu  Tab:pane"
  | "changes" -> "j/k:move  h/l:fold  s:stage  d:discard  c:commit  Space:menu  Tab:pane"
  | "diff" -> "j/k:move  n/p:page  h/l:pan  s/u:±hunk  Space:menu  Tab:pane"
  | _ -> "Space:menu  Tab:focus  Ctrl-C:quit"
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
  let modal_model, inject_modal = Modal.Component.state graph in
  let modal_controls = Modal.Component.controls ~inject:inject_modal in
  (* The destructive discard arrives at the pane pre-wrapped in the confirm
     modal; commit opens the message prompt. *)
  let discard =
    let%arr modal_controls and discard_path = data.discard_path in
    fun path ->
      modal_controls.Modal.Component.Controls.confirm
        ~title:"Discard changes?"
        ~body:(path ^ "  (cannot be undone)")
        ~on_confirm:(discard_path path)
  in
  let commit =
    let%arr modal_controls and commit = data.commit in
    modal_controls.Modal.Component.Controls.prompt
      ~title:"Commit message"
      ~on_submit:commit
  in
  let layout_tree = layout_tree ~data ~discard ~commit in
  let layout_model, layout_inject = Layout.Component.state layout_tree graph in
  let controls = Layout.Component.controls ~inject:layout_inject in
  let screen =
    Layout.Component.component layout_tree ~model:layout_model ~inject:layout_inject
  in
  let with_menu =
    Menu.Component.component
      ~commands:(commands ~layout:controls ~refresh:data.refresh ~commit)
      screen
  in
  (* Modal outermost: while one is open, Space must not open the menu. *)
  let with_modal = Modal.Component.component ~model:modal_model ~inject:inject_modal with_menu in
  let ~view:screen_view, ~handler = with_modal ~dimensions:screen_dimensions graph in
  let view =
    let%arr screen_view
    and dimensions
    and layout_model
    and branch = data.branch
    and notice = data.notice in
    let branch_info =
      match (branch : Git.Status.Branch.t option) with
      | None -> ""
      | Some { head; ahead; behind } ->
        let counts =
          (if ahead > 0 then sprintf " +%d" ahead else "")
          ^ if behind > 0 then sprintf " -%d" behind else ""
        in
        sprintf " %s%s" head counts
    in
    View.vcat
      [ screen_view
      ; Status_bar.render
          ~left:(" gitter " ^ branch_info)
          ~notice
          ~right:(hints ~focused:layout_model.Layout.State.Model.focused ^ " ")
          ~width:dimensions.width
      ]
  in
  ~view, ~handler
;;
