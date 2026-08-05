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
  type t =
    | Leaf of
        { id : string
        ; title : string Bonsai.t (* dynamic: e.g. the selected file's path *)
        ; component : Widget.t
        }
    | Split of
        { axis : Geometry.Axis.t
        ; children : (float * t) list
        }

  let leaf ~id ~title component = Leaf { id; title; component }
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
    | Leaf { id; title; component } -> [ id, title, component ]
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
    { Dimensions.width = Int.max 1 (rect.width - 2)
    ; height = Int.max 1 (rect.height - 2)
    }
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

let instantiate_leaves ~solved leaves (local_ graph) =
  (* Bound rather than returned directly: in tail position the closure below
     (which captures the local [graph]) would need to outlive this
     function's region, which the mode checker rejects. *)
  let instantiated =
    List.map leaves ~f:(fun (id, title, component) ->
      let dims =
        let%arr solved in
        leaf_dimensions ~solved id
      in
      let ~view, ~handler = component ~dimensions:dims graph in
      id, title, view, handler)
  in
  instantiated
;;

(* Layout's state is created by the caller (see [state]) rather than
   internally, so the caller can derive [Controls] from the same inject and
   hand them to other layers — e.g. Space-menu commands that drive focus and
   zoom. *)
let state ?(initially_hidden = String.Set.empty) (tree : Tree.t) (local_ graph) =
  let leaf_ids = List.map (Tree.leaves tree) ~f:(fun (id, _, _) -> id) in
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
    }
end

let controls ~inject =
  let%arr inject in
  { Controls.focus_next = inject State.Action.Focus_next
  ; toggle_zoom = inject State.Action.Toggle_zoom
  ; toggle_visible = (fun id -> inject (State.Action.Toggle_visible id))
  }
;;

let component (tree : Tree.t) ~model ~inject : Widget.t =
  fun ~dimensions (local_ graph) ->
  let leaf_list = Tree.leaves tree in
  let solved =
    let%arr dimensions and model in
    solve ~tree ~model ~dimensions
  in
  let instantiated = instantiate_leaves ~solved leaf_list graph in
  let leaf_views =
    all
      (List.map instantiated ~f:(fun (id, title, view, _) ->
         let%arr view and title in
         id, title, view))
  in
  let leaf_handlers =
    all
      (List.map instantiated ~f:(fun (id, _, _, handler) ->
         let%arr handler in
         id, handler))
  in
  let view =
    let%arr solved and leaf_views and model and dimensions in
    Render.render ~solved ~leaf_views ~focused_id:model.focused ~dimensions
  in
  let handler =
    let%arr model and inject and solved and leaf_handlers in
    Events.handle_event ~model ~inject ~solved ~handlers:leaf_handlers
  in
  ~view, ~handler
;;
