(* The bonsai_term frame loop sleeps until the next event (key, timer,
   resize) — a [Bonsai.Expert.Var] update alone does not produce a frame.
   Background producers (the embedded terminal's pty reader) call [wake]
   after publishing so their update paints now instead of riding the next
   keystroke or poller tick. bin/main installs the hook once the driver
   exists; it sends a no-op incoming event into the driver's queue. *)

let hook = ref (fun () -> ())
let set f = hook := f
let wake () = !hook ()
