open! Core
open! Bonsai_term

(** [ctrl c event] holds iff [event] is Ctrl-[c] in any form notty
    delivers it (uppercase ASCII, lowercase ASCII, or a [Uchar]). The
    quirk lives here so chord sites cannot drift out of agreement on
    which forms they cover. *)
val ctrl : char -> Event.t -> bool
