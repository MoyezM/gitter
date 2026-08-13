open! Core
open! Bonsai_term

(** The search line a pane renders into its enclosing BOTTOM border
    (R1-R5): the prompt/register left-aligned one cell in, the counts
    right-aligned in the same row. Both pieces float over the existing
    border glyphs (zcat transparency), so nothing here draws dashes.

    [width] is the pane's inner width — exactly the border span between
    the two corners. Degradation on narrow panes: the counts drop
    first, then the query tail-truncates behind a leading ellipsis so
    the cursor end stays visible. [None] when the prompt is idle. *)

val view : prompt:Prompt.t -> counts:string option -> width:int -> View.t option
