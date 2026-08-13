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

let layout_tree ~(data : Git_data.t) ~copy_path =
  let diff_title =
    let%arr selection = data.selection in
    match selection with
    | Some (path, `Staged) -> path ^ " (staged)"
    | Some (path, `Unstaged) -> path
    | Some (path, `Committed base) -> sprintf "%s (vs %s)" path base
    | None -> "Diff"
  in
  let stack_status =
    let%arr load = data.load
    and stack = data.stack in
    match load with
    | Git_data.Load.Not_loaded | Loading -> `Loading
    | Loaded _ ->
      (match stack with
       | Error e -> `Error e
       | Ok [] -> `Empty "no branch stack"
       | Ok branches -> `Stack branches)
  in
  (* +/- line totals on the right of the section title bars; hidden when
     the side has no changes. *)
  let counts_view counts =
    let%arr counts in
    let added, removed =
      Map.fold counts ~init:(0, 0) ~f:(fun ~key:_ ~data:(a, d) (ta, td) ->
        ta + a, td + d)
    in
    if added = 0 && removed = 0
    then None
    else
      Some
        (View.hcat
           [ View.text " "
           ; View.text ~attrs:[ Attr.fg Theme.green ] (sprintf "+%d" added)
           ; View.text " "
           ; View.text ~attrs:[ Attr.fg Theme.red ] (sprintf "-%d" removed)
           ; View.text " "
           ])
  in
  (* A section pane is its assembled [Input] plus chrome (id/titles). *)
  let files_pane ~id ~title ~title_right input =
    Layout.Component.Tree.leaf ~id ~title ~title_right (Panes.Files.Component.component input)
  in
  let committed_title =
    let%arr base = data.base in
    match base with
    | Some b -> "Committed vs " ^ b
    | None -> "Committed"
  in
  (* The committed title bar carries counts plus review progress. *)
  let committed_title_right =
    let%arr counts = counts_view data.committed.Panes.Files.Component.Input.counts
    and reviewed, total = data.review_progress in
    let progress =
      if total = 0
      then None
      else
        Some
          (View.text
             ~attrs:(if reviewed = total then [ Attr.fg Theme.green ] else Theme.context)
             (sprintf "\u{2713}%d/%d " reviewed total))
    in
    match counts, progress with
    | None, None -> None
    | Some c, None -> Some c
    | None, Some p -> Some (View.hcat [ View.text " "; p ])
    | Some c, Some p -> Some (View.hcat [ c; p ])
  in
  Layout.Component.Tree.(
    split
      `Row
      [ ( 1.
        , split
            `Col
            [ ( 1.
              , files_pane
                  ~id:"committed"
                  ~title:committed_title
                  ~title_right:committed_title_right
                  data.committed )
            ; ( 1.
              , files_pane
                  ~id:"staged"
                  ~title:(Bonsai.return "Staged")
                  ~title_right:
                    (counts_view data.staged.Panes.Files.Component.Input.counts)
                  data.staged )
            ; ( 2.
              , files_pane
                  ~id:"changes"
                  ~title:(Bonsai.return "Changes")
                  ~title_right:
                    (counts_view data.unstaged.Panes.Files.Component.Input.counts)
                  data.unstaged )
            ; ( 1.
              , leaf
                  ~id:"stack"
                  ~title:(Bonsai.return "Stack")
                  (Panes.Stack.Component.component
                     ~status:stack_status
                     ~base:data.base
                     ~set_base:data.set_base) )
            ] )
      ; ( 2.
        , leaf
            ~id:"diff"
            ~title:diff_title
            (Panes.Diff.Component.component
               ~selection:data.selection
               ~revision:data.revision
               ~stage_hunk:data.stage_hunk
               ~unstage_hunk:data.unstage_hunk
               ~copy_path) )
      ])
;;

let commands ~(layout : Layout.Component.Controls.t Bonsai.t) ~refresh ~commit ~terminal =
  let%arr layout and refresh and commit and terminal in
  [ Menu.Commands.Group
      { key = 'w'
      ; label = "window"
      ; children =
          [ Action { key = 'z'; label = "zoom"; effect = layout.toggle_zoom }
          ; Action { key = 'n'; label = "next pane"; effect = layout.focus_next }
          ; Action { key = 's'; label = "toggle stack"; effect = layout.toggle_visible "stack" }
          ; Action { key = 'c'; label = "toggle committed"; effect = layout.toggle_visible "committed" }
          ; Action
              { key = 'r'
              ; label = "review layout"
              ; effect = layout.set_hidden (String.Set.of_list [ "staged"; "changes" ])
              }
          ; Action
              { key = 'w'
              ; label = "work layout"
              ; effect = layout.set_hidden (String.Set.of_list [ "stack"; "committed" ])
              }
          ]
      }
  ; Menu.Commands.Group
      { key = 'g'
      ; label = "git"
      ; children =
          [ Action { key = 'r'; label = "reload status"; effect = refresh }
          ; Action { key = 'c'; label = "commit"; effect = commit }
          ; Action { key = 't'; label = "terminal"; effect = terminal }
          ]
      }
  ]
;;

(* Ctrl-C quits — but only when the event reaches this layer. The shell
   overlay wraps OUTSIDE it and, while visible, forwards Ctrl-C to the
   shell as SIGINT (with a hint pointing at Ctrl-T), so quitting requires
   leaving the terminal first. Replaces
   [Bonsai_term.Loop.make_app_exit_on_ctrlc]. *)
let exit_on_ctrlc ~exit (base : Widget.screen) : Widget.screen =
  let handler =
    let%arr base_handler = base.Widget.handler in
    fun (event : Event.t) ->
      if Chord.ctrl 'c' event then exit () else base_handler event
  in
  { base with Widget.handler }
;;

let app ~exit ~(dimensions : Dimensions.t Bonsai.t) (local_ graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let screen_dimensions =
    let%arr dimensions in
    { Dimensions.height = Int.max 0 (dimensions.height - 1)
    ; width = dimensions.width
    }
  in
  let modal_model, inject_modal = Modal.Component.state graph in
  let modal_controls = Modal.Component.controls ~inject:inject_modal in
  (* Status-bar notifications: ONE slot, the bottom-right (where the key
     hints live). [notify] takes the slot over for 5s, then it falls back
     to the hints — which are derived live from the focused pane, so
     whatever they say at expiry is what returns. A newer notification
     restarts the window (the generation guard disarms stale timers). *)
  let notice, set_notice = Bonsai.state None graph in
  (* The expiry timer is a latest-wins run: a newer notification's bump
     disarms a stale timer's clear. *)
  let notice_latest = Latest.create () in
  let notify =
    let%arr set_notice in
    fun kind message ->
      let%bind.Effect () = set_notice (Some (message, kind)) in
      Latest.run
        notice_latest
        ~fetch:(fun () -> Async.Clock_ns.after (Time_ns.Span.of_sec 5.))
        ~commit:(fun () -> set_notice None)
  in
  (* Git_data's interface: Some = post an error, None = an op succeeded,
     drop any stale error right away (and disarm its timer). *)
  let set_error_notice =
    let%arr notify and set_notice in
    function
    | Some message -> notify `Error message
    | None -> Effect.Many [ Latest.invalidate notice_latest; set_notice None ]
  in
  (* The destructive discard is pre-wrapped in the confirm modal — handed
     to Git_data, which owns the per-section op wiring. *)
  let discard_confirm =
    let%arr modal_controls in
    fun ~path ~discard ->
      modal_controls.Modal.Component.Controls.confirm
        ~title:"Discard changes?"
        ~body:(path ^ "  (cannot be undone)")
        ~on_confirm:discard
  in
  (* Copy the selected path (repo-root-relative, as git reports it):
     native tool first, OSC 52 escape to the hosting terminal when none
     exists (headless/SSH). *)
  let copy_path =
    let write_to_tty = Bonsai_term.Expert.Write_to_tty.write_string_to_tty graph in
    let%arr write_to_tty and notify in
    fun path ->
      let%bind.Effect copied =
        Effect.of_deferred_thunk (fun () -> Clipboard.copy_via_tool path)
      in
      match copied with
      | Ok () -> notify `Info ("copied " ^ path)
      | Error (_ : Error.t) ->
        Effect.Many [ write_to_tty (Clipboard.osc52 path); notify `Info ("copied " ^ path) ]
  in
  (* The commit-message prompt, handed to Git_data as an op factory (it
     wires [c] through the section machines — burst-safe) and reused for
     the menu entry below. *)
  let commit_confirm =
    let%arr modal_controls in
    fun ~commit ->
      modal_controls.Modal.Component.Controls.prompt ~title:"Commit message" ~on_submit:commit
  in
  let data =
    Git_data.create
      ~discard_confirm
      ~commit_confirm
      ~copy_path
      ~set_notice:set_error_notice
      graph
  in
  let layout_tree = layout_tree ~data ~copy_path in
  (* The stack and committed panes start hidden — Space w s / w c. *)
  let layout_model, layout_inject =
    Layout.Component.state
      ~initially_hidden:(String.Set.of_list [ "stack"; "committed" ])
      layout_tree
      graph
  in
  let controls = Layout.Component.controls ~inject:layout_inject in
  (* The layout IS the screen: every combinator below is
     [screen -> screen] over the instantiated record, so the search
     surface and the focused pane's hints ride to the top with no side
     channels, and nothing at screen level discards dimensions. *)
  let screen =
    Layout.Component.component
      layout_tree
      ~model:layout_model
      ~inject:layout_inject
      ~dimensions:screen_dimensions
      graph
  in
  let term =
    Term_pane.Component.create
      ~dimensions:screen_dimensions
      ~on_show:screen.Widget.commit_search_prompts
      ~on_hide:data.refresh
      ~on_ctrl_c:
        (let%arr notify in
         notify `Info "Ctrl-T leaves the terminal — then Ctrl-C quits gitter")
      graph
  in
  let term_controls = Term_pane.Component.controls term in
  let with_menu =
    Menu.Component.component
      ~commands:
        (commands
           ~layout:controls
           ~refresh:data.refresh
           ~commit:data.commit_prompt
           ~terminal:
             (let%arr c = term_controls in
              c.Term_pane.Component.Controls.toggle))
      screen
      ~dimensions:screen_dimensions
      graph
  in
  (* Stack: Modal outermost (nothing may open over it), then the shell
     overlay, then Ctrl-C-quit, then the menu and panes. Exit sits BELOW
     the overlay so a visible terminal captures Ctrl-C for the shell; a
     side effect is that an open modal also swallows Ctrl-C (Esc first). *)
  let with_exit = exit_on_ctrlc ~exit with_menu in
  let with_term = Term_pane.Component.wrap term with_exit in
  let with_modal =
    Modal.Component.component
      ~model:modal_model
      ~inject:inject_modal
      with_term
      ~dimensions:screen_dimensions
      graph
  in
  let view =
    let%arr screen_view = with_modal.Widget.view
    and hints = with_modal.Widget.hints
    and dimensions
    and branch = data.branch
    and notice in
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
          ~right:(hints ^ " ")
          ~width:dimensions.width
      ]
  in
  ~view, ~handler:with_modal.Widget.handler
;;
