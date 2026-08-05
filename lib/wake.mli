(** Wake the bonsai_term frame loop from outside its event sources — see
    wake.ml. [wake] is a no-op until [set] installs the driver hook. *)

val set : (unit -> unit) -> unit
val wake : unit -> unit
