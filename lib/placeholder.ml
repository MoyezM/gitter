open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* A stand-in leaf that proves the plumbing: it shows its allotted size and
   echoes the last event it received, so we can see that dimensions and
   events actually reach the leaf through however many wrappers surround
   it. *)

let component ~name : Widget.t =
  fun ~dimensions (local_ graph) ->
  let last_event, set_last_event = Bonsai.state "(none yet)" graph in
  let view =
    let%arr dimensions and last_event in
    (* Honor the contract: never render wider than the allotment
       ([View.center] grows to fit oversized content rather than clipping). *)
    let fit s = String.prefix s (Int.max 1 dimensions.width) in
    View.center
      ~within:dimensions
      (View.vcat
         [ View.text ~attrs:Theme.header (fit name)
         ; View.text
             ~attrs:Theme.context
             (fit (sprintf "%dx%d" dimensions.width dimensions.height))
         ; View.text ~attrs:Theme.context (fit ("last event: " ^ last_event))
         ])
  in
  let handler =
    let%arr set_last_event in
    fun (event : Event.t) ->
      set_last_event (Sexp.to_string ([%sexp_of: Event.t] event))
  in
  ~view, ~handler
;;
