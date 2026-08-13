open! Core
open! Bonsai_term

(* The UI contracts between the layout, its leaves, and the screen-level
   combinators.

   (Named [Widget] rather than [Component] so that combinator folders can
   have their own [component.ml] wiring file without shadowing this
   module.) *)

(* A layout LEAF: a pane's content plus what it contributes upward — one
   line into the layout-drawn frame ([border] — the search
   prompt/register, see Search.Border), its search surface
   ([search_active], and [commit_search], the implicit commit), and its
   status-bar key hints. The layout instantiates leaves, so it can see
   these outputs — that is what lets it own the whole L3 contract
   (commit prompts on focus steals, gate the Space menu) with no
   root-side mirror channels, and show the focused pane's hints without
   a hand-maintained table at the root. A leaf must render within the
   dimensions it is given and never assumes where it sits on screen —
   the layout translates mouse coordinates so leaves stay
   position-independent. *)
type pane =
  { view : View.t Bonsai.t
  ; border : View.t option Bonsai.t
  ; search_active : bool Bonsai.t (* this pane's prompt is open *)
  ; commit_search : unit Effect.t Bonsai.t
  ; handler : (Event.t -> unit Effect.t) Bonsai.t
  ; hints : string (* the pane's key hints, shown while it is focused *)
  }

type leaf = dimensions:Dimensions.t Bonsai.t -> local_ Bonsai.graph -> pane

(* The SCREEN: the instantiated top level. The layout produces one;
   screen combinators (menu, shell overlay, Ctrl-C exit, modal) are
   [screen -> screen], wrapping [view]/[handler] and passing the search
   surface and hints through — so the root layers read them off whatever
   the outermost layer is, and nothing at screen level re-instantiates
   or discards dimensions. *)
type screen =
  { view : View.t Bonsai.t
  ; handler : (Event.t -> unit Effect.t) Bonsai.t
  ; search_active : bool Bonsai.t (* some pane's prompt is open *)
  ; commit_search_prompts : unit Effect.t Bonsai.t
    (* implicitly commit every active prompt (screen-taking actions
       outside the layout's own routing, e.g. Ctrl-T) *)
  ; hints : string Bonsai.t (* the focused pane's key hints *)
  }
