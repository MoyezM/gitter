open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The Layout combinator: a split tree of components in, one component out.

   Geometry is solved once per frame by [Solver]; [Render] and [Events] both
   read that single result. Children are told their inner rectangle via
   their [dimensions] input and receive mouse events translated into their
   own coordinate space (see [Events]), so leaves stay position-independent.

   This file is only the graph wiring; the logic lives in the sibling
   modules. *)

module Tree = struct
  type leaf =
    { id : string
    ; title : string Bonsai.t (* dynamic: e.g. the selected file's path *)
    ; title_right : View.t option Bonsai.t
      (* right-aligned title-bar extra, e.g. +/- diffstat counts *)
    ; component : Widget.leaf
      (* panes contribute a bottom-border line and their search surface
         (see Widget.pane) - the layout reads both off instantiation *)
    }

  type t =
    | Leaf of leaf
    | Split of
        { axis : Geometry.Axis.t
        ; children : (float * t) list
        }

  let leaf ?(title_right = Bonsai.return None) ~id ~title component =
    Leaf { id; title; title_right; component }
  ;;

  let split axis children = Split { axis; children }

  (* Desugars to the solver's binary core with hidden leaves pruned; a
     split whose children all pruned disappears, and [Solver.Tree.node]'s
     single-child collapse removes degenerate splits (and their dividers)
     for free. See [Solver.Tree.node]. *)
  let rec to_solver ~hidden = function
    | Leaf { id; _ } -> if Set.mem hidden id then None else Some (Solver.Tree.Leaf id)
    | Split { axis; children } ->
      (match
         List.filter_map children ~f:(fun (w, c) ->
           to_solver ~hidden c |> Option.map ~f:(fun c -> w, c))
       with
       | [] -> None
       | children -> Some (Solver.Tree.node axis children))
  ;;

  let rec leaves = function
    | Leaf leaf -> [ leaf ]
    | Split { children; _ } -> List.concat_map children ~f:(fun (_, c) -> leaves c)
  ;;
end

let solve ~tree ~(model : State.Model.t) ~(dimensions : Dimensions.t) =
  let solver_tree =
    if model.zoomed
    then Solver.Tree.Leaf model.focused
    else
      (* [Toggle_visible] never hides the last leaf, so the fallback is
         unreachable in practice — but stay total. *)
      Tree.to_solver ~hidden:model.hidden tree
      |> Option.value ~default:(Solver.Tree.Leaf model.focused)
  in
  Solver.solve
    solver_tree
    ~fractions:model.fractions
    ~rect:{ x = 0; y = 0; width = dimensions.width; height = dimensions.height }
;;

let leaf_dimensions ~(solved : Solver.Solved.t) id =
  match List.find solved.leaves ~f:(fun l -> String.equal l.id id) with
  | Some { rect; _ } ->
    (* [Rect.inner] is the frame-inset law; leaves additionally assume
       dimensions of at least one cell. *)
    let inner = Geometry.Rect.inner rect in
    { Dimensions.width = Int.max 1 inner.width; height = Int.max 1 inner.height }
  | None ->
    (* Not on screen (e.g. unfocused leaves while zoomed). *)
    { Dimensions.width = 1; height = 1 }
;;

(* [Bonsai.all]-style combining without depending on its exact API. *)
let all xs =
  List.fold_right xs ~init:(Bonsai.return []) ~f:(fun x acc ->
    let%arr x and acc in
    x :: acc)
;;

(* One leaf, instantiated: the declaration's slots plus the pane's
   outputs, nested whole — a new [Widget.pane] field needs no mirror
   here. *)
type instantiated =
  { id : string
  ; title : string Bonsai.t
  ; title_right : View.t option Bonsai.t
  ; pane : Widget.pane
  }

let instantiate_leaves ~solved leaves (local_ graph) =
  (* Bound rather than returned directly: in tail position the closure below
     (which captures the local [graph]) would need to outlive this
     function's region, which the mode checker rejects. *)
  let instantiated =
    List.map leaves ~f:(fun { Tree.id; title; title_right; component } ->
      let dims =
        let%arr solved in
        leaf_dimensions ~solved id
      in
      { id; title; title_right; pane = component ~dimensions:dims graph })
  in
  instantiated
;;

(* Layout's state is created by the caller (see [state]) rather than
   internally, so the caller can derive [Controls] from the same inject and
   hand them to other layers — e.g. Space-menu commands that drive focus and
   zoom. *)
let state ?(initially_hidden = String.Set.empty) (tree : Tree.t) (local_ graph) =
  let leaf_ids = List.map (Tree.leaves tree) ~f:(fun (l : Tree.leaf) -> l.id) in
  let first_leaf =
    List.find leaf_ids ~f:(fun id -> not (Set.mem initially_hidden id))
    |> Option.value ~default:(List.hd_exn leaf_ids)
  in
  Bonsai.state_machine
    ~default_model:(State.initial ~hidden:initially_hidden ~first_leaf ())
    ~apply_action:(State.apply_action ~leaf_ids)
    graph
;;

(* The control handle: effects other layers may use to drive the layout. *)
module Controls = struct
  type t =
    { focus_next : unit Effect.t
    ; toggle_zoom : unit Effect.t
    ; toggle_visible : string -> unit Effect.t
    ; set_hidden : String.Set.t -> unit Effect.t (* visibility preset *)
    }
end

let controls ~inject =
  let%arr inject in
  { Controls.focus_next = inject State.Action.Focus_next
  ; toggle_zoom = inject State.Action.Toggle_zoom
  ; toggle_visible = (fun id -> inject (State.Action.Toggle_visible id))
  ; set_hidden = (fun hidden -> inject (State.Action.Set_hidden hidden))
  }
;;

(* The instantiated layout IS the screen ([Widget.screen]): besides the
   view and handler it exposes the search surface and the focused pane's
   hints — the layout is the only place that can see every pane's
   prompt, and the place that owns focus, so the L3 contract (commit the
   focused pane's prompt on focus steals, tell the menu when Space is a
   printable) lives here with no side channels, and the hints follow
   focus without a root-side table. *)
let component (tree : Tree.t) ~model ~inject ~dimensions (local_ graph) : Widget.screen =
  let leaf_list = Tree.leaves tree in
  let solved =
    let%arr dimensions and model in
    solve ~tree ~model ~dimensions
  in
  let instantiated = instantiate_leaves ~solved leaf_list graph in
  let leaf_views =
    all
      (List.map instantiated ~f:(fun { id; title; title_right; pane; _ } ->
         let%arr view = pane.Widget.view
         and title
         and title_right
         and border = pane.Widget.border in
         { Render.Leaf_view.id; title; title_right; bottom = border; inner = view }))
  in
  let leaf_handlers =
    all
      (List.map instantiated ~f:(fun { id; pane; _ } ->
         let%arr handler = pane.Widget.handler in
         id, handler))
  in
  let leaf_commits =
    all
      (List.map instantiated ~f:(fun { id; pane; _ } ->
         let%arr commit_search = pane.Widget.commit_search in
         id, commit_search))
  in
  let search_active =
    let actives = all (List.map instantiated ~f:(fun { pane; _ } -> pane.Widget.search_active)) in
    let%arr actives in
    List.exists actives ~f:Fn.id
  in
  let commit_search_prompts =
    let%arr leaf_commits in
    Effect.Many (List.map leaf_commits ~f:snd)
  in
  let view =
    let%arr solved and leaf_views and model and dimensions in
    Render.render ~solved ~leaf_views ~focused_id:model.focused ~dimensions
  in
  let handler =
    let%arr model and inject and solved and leaf_handlers and leaf_commits in
    Events.handle_event
      ~model
      ~inject
      ~solved
      ~handlers:leaf_handlers
      ~commits:leaf_commits
  in
  let hints =
    (* Static per leaf, so only focus changes recompute. The default is
       unreachable in practice: focus always names a leaf. Tab is the
       LAYOUT's binding (see [Events.handle_event]) — appended here
       once, not restated by every leaf. *)
    let by_id = List.map instantiated ~f:(fun { id; pane; _ } -> id, pane.Widget.hints) in
    let%arr model in
    (List.Assoc.find by_id model.State.Model.focused ~equal:String.equal
     |> Option.value ~default:"")
    ^ "  Tab:pane"
  in
  { Widget.view; handler; search_active; commit_search_prompts; hints }
;;
