open! Core

(** The selection-repair law shared by the list panes — see selection.ml:
    keep a surviving key; a vanished key moves to the nearest surviving
    successor in the old order, else predecessor, else [None]. *)
val repair
  :  old_keys:string list
  -> selection:string option
  -> new_keys:string list
  -> string option
