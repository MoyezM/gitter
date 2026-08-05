open! Core
open! Bonsai_term

(** Pure View production for the diff pane. *)

type content =
  [ `Message of string
  | `Document of Document.t * Highlight.t * Highlight.t
  ]

(** Freshness owner: only a result tagged with the CURRENT selection's path
    renders as a document; a mismatched or missing result is "loading". *)
val content : selection:string option -> result:Fetch.result -> content

(** Renders the viewport slice: scroll clamped via [State.clamp_scroll], the
    cursor row from [State.effective_cursor], one highlight window per
    visible hunk run. Every row is exactly [dimensions.width] wide. *)
val render : content:content -> model:State.Model.t -> dimensions:Dimensions.t -> View.t
