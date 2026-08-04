open! Core
open! Bonsai_term

(* The universal UI contract. Every renderable unit — a pane's content, a
   menu, a whole screen — is a widget: a function from its allotted
   dimensions to a (view, handler) pair, the same shape bonsai_term expects
   of an app.

   Mode, Layout, and the Space menu are combinators over this type: they
   take widgets and return widgets, so they nest in any order and know
   nothing about their contents. A widget must render within the dimensions
   it is given and never assumes where it sits on screen — the enclosing
   layer translates mouse coordinates so leaves stay position-independent.

   (Named [Widget] rather than [Component] so that combinator folders can
   have their own [component.ml] wiring file without shadowing this
   module.) *)

type pair = view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
type t = dimensions:Dimensions.t Bonsai.t -> local_ Bonsai.graph -> pair
