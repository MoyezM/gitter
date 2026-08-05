open! Core
open Async

(* [start_with_driver] instead of [start]: the driver handle is what lets
   background work (the embedded terminal's pty reader) wake the frame loop
   via [Gitter.Wake] — see lib/wake.ml. Ctrl-C exit is re-implemented here
   because plain [start] added it for us. *)
let run () =
  let%bind.Deferred.Or_error driver =
    Bonsai_term.start_with_driver
      (* 120 covers ProMotion displays; frames are cheap (windowed highlight
         queries run 0.1–0.4ms) so the higher tick just means wheel events
         batch less and scrolling renders more continuously. *)
      ~target_frames_per_second:120.
      ~get_view_and_handler:Fn.id
      ~handle_incoming:(fun _ () -> Bonsai_term.Effect.Ignore)
      (fun ~exit ~dimensions (local_ graph) ->
        let ~view, ~handler = Gitter.App.app ~dimensions graph in
        let handler =
          let open Bonsai.Let_syntax in
          let%arr handler in
          fun (event : Bonsai_term.Event.t) ->
            match event with
            | Key_press { key = ASCII 'c'; mods = [ Ctrl ] } -> exit ()
            | event -> handler event
        in
        Bonsai_term.stitch (~view, ~handler))
  in
  Gitter.Wake.set (fun () -> Bonsai_term.Driver.send_incoming_event driver ());
  match%map Bonsai_term.Driver.finished driver with
  | Ok () | Error `Incoming_events_pipe_closed -> Ok ()
;;

let command =
  Async.Command.async_or_error
    ~summary:"gitter"
    (let%map_open.Command () = return () in
     run)
;;

let () = Command_unix.run command
