open! Core

(* The outermost wrapper: routes between named screens.

   Step 0 stub: there is exactly one screen, so this is the identity. It
   becomes a real router (a screen variant + [match%sub]) in Step 3, when a
   second screen exists to switch to. The contract — takes components,
   returns a component — is already its final shape. *)

let component (screen : Widget.t) : Widget.t = screen
