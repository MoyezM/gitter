open! Core
open! Bonsai_term

(* notty's Ctrl quirk, in one place: Ctrl-letters arrive as UPPERCASE
   ASCII — and sometimes as a [Uchar] — so a site matching only the
   [ASCII] form silently misses one delivery. Match chords through
   [ctrl] and every form is covered. *)

let ctrl c (event : Event.t) =
  let upper = Char.uppercase c
  and lower = Char.lowercase c in
  match event with
  | Key_press { key = ASCII k; mods = [ Ctrl ] } ->
    Char.equal k upper || Char.equal k lower
  | Key_press { key = Uchar u; mods = [ Ctrl ] } ->
    Uchar.equal u (Uchar.of_char upper) || Uchar.equal u (Uchar.of_char lower)
  | _ -> false
;;
