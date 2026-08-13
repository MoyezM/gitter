open! Core
open! Bonsai_term
open State

(* Event routing, as pure functions over the solved geometry. The precedence
   stack, top to bottom: layout keys -> drag protocol -> divider grab ->
   position-routed forwarding to leaves (mouse translated into the leaf's
   own coordinate space). *)

let forward_to_leaf ~handlers (leaf : Solver.Solved.leaf) (event : Event.t) =
  match List.Assoc.find handlers leaf.id ~equal:String.equal with
  | None -> Effect.Ignore
  | Some handler ->
    (match event with
     | Event.Mouse { kind; position; mods } ->
       (* Hit-testing uses the full rect (borders included); the leaf's
          handler speaks inner coordinates. [Rect.to_inner] owns the
          frame-inset law and drops clicks on the border/title, which
          would otherwise arrive as row -1 or column -1 and mutate leaf
          state. *)
       (match Geometry.Rect.to_inner leaf.rect ~x:position.x ~y:position.y with
        | None -> Effect.Ignore
        | Some (x, y) -> handler (Event.Mouse { kind; position = { Position.x; y }; mods }))
     | event -> handler event)
;;

let forward_to_focused ~handlers ~focused event =
  match List.Assoc.find handlers focused ~equal:String.equal with
  | Some handler -> handler event
  | None -> Effect.Ignore
;;

(* A drag reads as: the pointer's position along the split, as a fraction of
   the split's extent. The solver clamps, so this is a request, not an
   obligation. *)
let drag_frac ~(divider : Solver.Solved.divider) ~(position : Position.t) =
  let pointer =
    match divider.axis with
    | `Row -> position.x
    | `Col -> position.y
  in
  let start = Geometry.Axis.start divider.axis divider.split_rect in
  let total = Geometry.Axis.size divider.axis divider.split_rect in
  if total <= 0
  then None
  else Some (Float.of_int (pointer - start) /. Float.of_int total)
;;

let handle_drag
  ~inject
  ~(solved : Solver.Solved.t)
  ~path
  ~(kind : Event.mouse_kind)
  ~(position : Position.t)
  =
  match kind with
  | Release -> inject Action.End_drag
  | Drag ->
    (match List.find solved.dividers ~f:(fun d -> String.equal d.path path) with
     | None -> inject End_drag
     | Some divider ->
       (match drag_frac ~divider ~position with
        | Some frac -> inject (Set_frac (path, frac))
        | None -> inject End_drag))
  | _ -> Effect.Ignore
;;

(* The focused pane's implicit-commit effect — what a focus-stealing
   action fires before focus moves (L3). *)
let commit_focused ~commits ~focused =
  Option.value (List.Assoc.find commits focused ~equal:String.equal) ~default:Effect.Ignore
;;

let handle_mouse
  ~(model : Model.t)
  ~inject
  ~solved
  ~handlers
  ~commits
  ~(kind : Event.mouse_kind)
  ~(position : Position.t)
  event
  =
  match model.dragging with
  | Some path -> handle_drag ~inject ~solved ~path ~kind ~position
  | None ->
    (match kind, Solver.divider_at solved ~x:position.x ~y:position.y with
     | Left, Some d -> inject (Action.Start_drag d.path)
     | _ ->
       (match Solver.leaf_at solved ~x:position.x ~y:position.y with
        | None -> Effect.Ignore
        | Some leaf ->
          (match kind with
           (* Scrolling focuses too, not just clicking. A wheel event
              already routes to the leaf under the POINTER rather than the
              focused one, so without this, wheeling to bring something
              into view and then pressing a key sends that key to whatever
              pane still holds focus — silently, since the key is usually
              unbound there. *)
           | Left | Scroll _ ->
             (* Mouse into ANOTHER pane is a focus-stealing action: the
                focused pane's active prompt commits first (L3). A click
                inside the prompting pane instead forwards, so the pane
                can resolve it against what it rendered before
                committing. *)
             let commit =
               if String.equal leaf.id model.focused
               then []
               else [ commit_focused ~commits ~focused:model.focused ]
             in
             Effect.Many
               (commit @ [ inject (Focus leaf.id); forward_to_leaf ~handlers leaf event ])
           | _ -> forward_to_leaf ~handlers leaf event)))
;;

let handle_event ~model ~inject ~solved ~handlers ~commits (event : Event.t) =
  match event with
  | Event.Key_press { key = Tab; mods = [] } ->
    (* Tab is a focus-stealing action too (L3): the focused pane's
       active prompt implicitly commits, then focus moves — the same
       mechanism as mouse into another pane, so panes need no Tab
       handling of their own. *)
    Effect.Many [ commit_focused ~commits ~focused:model.Model.focused; inject Action.Focus_next ]
  | Key_press _ | Paste _ -> forward_to_focused ~handlers ~focused:model.Model.focused event
  | Mouse { kind; position; _ } ->
    handle_mouse ~model ~inject ~solved ~handlers ~commits ~kind ~position event
;;
