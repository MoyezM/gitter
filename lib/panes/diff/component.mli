open! Core
open! Bonsai_term

(** The diff pane: shows the selected file's change vs HEAD, syntax
    highlighted, with a helix-feel cursor (j/k/arrows, n/p and brackets for
    half-pages, wheel, click). *)

val component : selection:string option Bonsai.t -> Widget.t
