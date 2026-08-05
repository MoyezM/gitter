open! Core
open Async
module Session = Gitter.Terminal.Session

(* End-to-end pty round trip: keys go down the pty, the child echoes, the
   echo lands in the libvterm grid. *)

let check name cond = printf "%s %s\n" (if cond then "PASS" else "FAIL") name

let wait_for ?(timeout = 5.) f =
  let poll =
    Deferred.repeat_until_finished () (fun () ->
      if f ()
      then return (`Finished true)
      else (
        let%map () = Clock.after (Time_float.Span.of_ms 20.) in
        `Repeat ()))
  in
  match%map Clock.with_timeout (Time_float.Span.of_sec timeout) poll with
  | `Result r -> r
  | `Timeout -> false
;;

let main () =
  let closed = Ivar.create () in
  let session =
    Session.create
      ~command:"cat"
      ~cwd:"/tmp"
      ~rows:5
      ~cols:20
      ~publish:(fun () -> ())
      ~on_closed:(fun () -> Ivar.fill_if_empty closed ())
  in
  let vt = Session.vterm session in
  Session.send_event session (Key_press { key = ASCII 'h'; mods = [] });
  Session.send_event session (Key_press { key = ASCII 'i'; mods = [] });
  let%bind echoed =
    wait_for (fun () -> String.is_substring (Gitter.Terminal.Vterm.row_text vt 0) ~substring:"hi")
  in
  check "keys reach child and echo back" echoed;
  (* first Ctrl-D flushes the pending canonical line, second is EOF *)
  Session.send_event session (Key_press { key = ASCII 'd'; mods = [ Ctrl ] });
  Session.send_event session (Key_press { key = ASCII 'd'; mods = [ Ctrl ] });
  let%bind saw_close = Clock.with_timeout (Time_float.Span.of_sec 5.) (Ivar.read closed) in
  check
    "ctrl-d closes the child"
    (match saw_close with
     | `Result () -> true
     | `Timeout -> false);
  Session.close session;
  return ()
;;

let () =
  don't_wait_for
    (let%bind () = main () in
     exit 0);
  never_returns (Scheduler.go ())
;;
