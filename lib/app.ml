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

let layout_tree ~(data : Git_data.t) ~commit ~copy_path =
  let diff_title =
    let%arr selection = data.selection
    and base = data.base
    and review = data.review in
    match selection with
    | Some (path, `Staged) -> path ^ " (staged)"
    | Some (path, `Unstaged) -> path
    | Some (path, `Committed ((_ : string), (_ : string))) ->
      (* the key carries resolved shas; label with the ref NAMES *)
      (match review with
       | Some r -> sprintf "%s (%s vs %s)" path r.Git_data.head_ref r.base_ref
       | None -> sprintf "%s (vs %s)" path (Option.value base ~default:"base"))
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
  let no_reviews = Bonsai.return String.Set.empty in
  let files_pane ~id ~title ~title_right ~status ~counts ~reviewed ~side
        ?commit_override
        (section : Git_data.section_data)
    =
    let commit = Option.value commit_override ~default:commit in
    Layout.Component.Tree.leaf
      ~id
      ~title
      ~title_right
      (Panes.Files.Component.component
         ~status
         ~rows:section.rows
         ~cursor:section.cursor
         ~scroll:section.scroll
         ~counts
         ~reviewed
         ~side
         ~commit
         ~inject:section.inject)
  in
  (* The committed pane: this branch vs its base — or, in review mode,
     the chosen branch vs its parent. *)
  let committed_status =
    let%arr load = data.load
    and rows = data.committed.rows
    and base = data.base
    and review = data.review in
    let empty_message =
      match review, base with
      | Some r, _ -> Some (sprintf "nothing in %s vs %s" r.Git_data.head_ref r.base_ref)
      | None, Some b -> Some ("nothing committed vs " ^ b)
      | None, None -> None
    in
    match load, empty_message with
    | (Git_data.Load.Not_loaded | Loading), _ -> `Loading
    | Loaded _, None -> `Empty "no base branch"
    | Loaded _, Some m -> if List.is_empty rows then `Empty m else `Tree
  in
  let committed_title =
    let%arr base = data.base
    and review = data.review in
    match review with
    | Some r -> sprintf "Review %s vs %s" r.Git_data.head_ref r.base_ref
    | None ->
      (match base with
       | Some b -> "Committed vs " ^ b
       | None -> "Committed")
  in
  (* The committed title bar carries counts plus review progress. *)
  let committed_title_right =
    let%arr counts = counts_view data.committed_counts
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
                  ~status:committed_status
                  ~counts:data.committed_counts
                  ~reviewed:data.reviewed
                  ~side:`Committed
                  ~commit_override:
                    (* committing the LOCAL repo from inside another
                       branch's review would be a misfire *)
                    (let%arr commit and review = data.review in
                     match review with
                     | Some (_ : Git_data.review) -> Effect.Ignore
                     | None -> commit)
                  data.committed )
            ; ( 1.
              , files_pane
                  ~id:"staged"
                  ~title:(Bonsai.return "Staged")
                  ~title_right:
                    (counts_view
                       (let%arr stat = data.diffstat in
                        stat.Git_data.staged_lines))
                  ~status:(files_status data.staged.rows ~empty:"nothing staged")
                  ~counts:
                    (let%arr stat = data.diffstat in
                     stat.Git_data.staged_lines)
                  ~reviewed:no_reviews
                  ~side:`Staged
                  data.staged )
            ; ( 2.
              , files_pane
                  ~id:"changes"
                  ~title:(Bonsai.return "Changes")
                  ~title_right:
                    (counts_view
                       (let%arr stat = data.diffstat in
                        stat.Git_data.unstaged_lines))
                  ~status:(files_status data.unstaged.rows ~empty:"working tree clean")
                  ~counts:
                    (let%arr stat = data.diffstat in
                     stat.Git_data.unstaged_lines)
                  ~reviewed:no_reviews
                  ~side:`Unstaged
                  data.unstaged )
            ; ( 1.
              , leaf
                  ~id:"stack"
                  ~title:(Bonsai.return "Stack")
                  (Panes.Stack.Component.component
                     ~status:stack_status
                     ~base:data.base
                     ~review_branch:
                       (* the INTENT: instant marker feedback on r, no
                          fetch round-trip *)
                       (let%arr intent = data.review_intent in
                        Option.map intent ~f:fst)
                     ~set_base:data.set_base
                     ~set_review:data.set_review_target) )
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

let commands ~(layout : Layout.Component.Controls.t Bonsai.t) ~refresh ~commit ~terminal ~review_rev =
  let%arr layout and refresh and commit and terminal and review_rev in
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
          ; Action { key = 'v'; label = "review rev"; effect = review_rev }
          ]
      }
  ]
;;

(* Ctrl-C quits — but only when the event reaches this layer. The shell
   overlay wraps OUTSIDE it and, while visible, forwards Ctrl-C to the
   shell as SIGINT (with a hint pointing at Ctrl-T), so quitting requires
   leaving the terminal first. notty reports Ctrl-C as UPPERCASE ASCII
   'C' (sometimes as a Uchar) — match every form, same as
   [Bonsai_term.Loop.make_app_exit_on_ctrlc], which this replaces. *)
let exit_on_ctrlc ~exit (base : Widget.t) : Widget.t =
  fun ~dimensions (local_ graph) ->
  let ~view, ~handler:base_handler = base ~dimensions graph in
  let handler =
    let%arr base_handler in
    fun (event : Event.t) ->
      match event with
      | Key_press { key = ASCII ('C' | 'c'); mods = [ Ctrl ] } -> exit ()
      | Key_press { key = Uchar u; mods = [ Ctrl ] }
        when Uchar.equal (Uchar.of_char 'C') u || Uchar.equal (Uchar.of_char 'c') u ->
        exit ()
      | event -> base_handler event
  in
  ~view, ~handler
;;

(* Context hints for the status bar: the focused pane's keys. *)
let hints ~focused =
  match focused with
  | "committed" -> "j/k:move  r:reviewed  y:copy path  Space:menu  Tab:pane"
  | "staged" -> "j/k:move  u:unstage  c:commit  y:copy path  Space:menu  Tab:pane"
  | "changes" -> "j/k:move  s:stage  d:discard  c:commit  y:copy path  Tab:pane"
  | "diff" -> "j/k:move  n/p:page  h/l:pan  s/u:\u{00B1}hunk  y:copy path"
  | "stack" -> "j/k:move  h/l:fold  Enter:base  r/R:review  Space:menu  Tab:pane"
  | _ -> "Space:menu  Tab:focus  C-t:term  Ctrl-C:quit"
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
  let notice_generation = ref 0 in
  let notify =
    let%arr set_notice in
    fun kind message ->
      let%bind.Effect mine =
        Effect.of_thunk (fun () ->
          incr notice_generation;
          !notice_generation)
      in
      let%bind.Effect () = set_notice (Some (message, kind)) in
      let%bind.Effect () =
        Effect.of_deferred_thunk (fun () ->
          Async.Clock_ns.after (Time_ns.Span.of_sec 5.))
      in
      let%bind.Effect current = Effect.of_thunk (fun () -> !notice_generation) in
      if current = mine then set_notice None else Effect.Ignore
  in
  (* Git_data's interface: Some = post an error, None = an op succeeded,
     drop any stale error right away (and disarm its timer). *)
  let set_error_notice =
    let%arr notify and set_notice in
    function
    | Some message -> notify `Error message
    | None ->
      let%bind.Effect () = Effect.of_thunk (fun () -> incr notice_generation) in
      set_notice None
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
  let data =
    Git_data.create ~discard_confirm ~copy_path ~set_notice:set_error_notice graph
  in
  let commit =
    let%arr modal_controls and commit = data.commit in
    modal_controls.Modal.Component.Controls.prompt
      ~title:"Commit message"
      ~on_submit:commit
  in
  (* Review any rev by name (origin/foo, a sha, a tag) — the head is
     used literally, based against the trunk. *)
  let review_rev =
    let%arr modal_controls and set_review_rev = data.set_review_rev in
    modal_controls.Modal.Component.Controls.prompt
      ~title:"Review rev (branch, origin/branch, sha)"
      ~on_submit:set_review_rev
  in
  let term =
    Term_pane.Component.create
      ~dimensions:screen_dimensions
      ~on_hide:data.refresh
      ~on_ctrl_c:
        (let%arr notify in
         notify `Info "Ctrl-T leaves the terminal — then Ctrl-C quits gitter")
      graph
  in
  let term_controls = Term_pane.Component.controls term in
  let layout_tree = layout_tree ~data ~commit ~copy_path in
  (* The stack and committed panes start hidden — Space w s / w c. *)
  let layout_model, layout_inject =
    Layout.Component.state
      ~initially_hidden:(String.Set.of_list [ "stack"; "committed" ])
      layout_tree
      graph
  in
  let controls = Layout.Component.controls ~inject:layout_inject in
  (* Entering review mode reveals the committed pane (it starts hidden):
     pressing r in the stack pane must SHOW the review, not set it
     invisibly. Watches the INTENT (synchronous with the keypress), not
     the resolved review, which only lands after a fetch. Leaving review
     mode leaves the layout alone. *)
  Bonsai.Edge.on_change
    ~equal:[%equal: (string * bool) option]
    ~callback:
      (let%arr controls in
       function
       | None -> Effect.Ignore
       (* keyed on the FULL intent: r->R on one branch changes only the
          flag and must still re-reveal a pane hidden in between *)
       | Some ((_ : string), (_ : bool)) ->
         controls.Layout.Component.Controls.show "committed")
    data.review_intent
    graph;
  let screen =
    Layout.Component.component layout_tree ~model:layout_model ~inject:layout_inject
  in
  let with_menu =
    Menu.Component.component
      ~commands:
        (commands
           ~layout:controls
           ~refresh:data.refresh
           ~commit
           ~review_rev
           ~terminal:
             (let%arr c = term_controls in
              c.Term_pane.Component.Controls.toggle))
      screen
  in
  (* Stack: Modal outermost (nothing may open over it), then the shell
     overlay, then Ctrl-C-quit, then the menu and panes. Exit sits BELOW
     the overlay so a visible terminal captures Ctrl-C for the shell; a
     side effect is that an open modal also swallows Ctrl-C (Esc first). *)
  let with_exit = exit_on_ctrlc ~exit with_menu in
  let with_term = Term_pane.Component.wrap term with_exit in
  let with_modal =
    Modal.Component.component ~model:modal_model ~inject:inject_modal with_term
  in
  let ~view:screen_view, ~handler = with_modal ~dimensions:screen_dimensions graph in
  let view =
    let%arr screen_view
    and dimensions
    and layout_model
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
          ~right:(hints ~focused:layout_model.Layout.State.Model.focused ^ " ")
          ~width:dimensions.width
      ]
  in
  ~view, ~handler
;;
