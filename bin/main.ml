open! Core

let command =
  Async.Command.async_or_error
    ~summary:"gitter"
    (let%map_open.Command () = return () in
     (* 120 covers ProMotion displays; frames are cheap (windowed highlight
        queries run 0.1–0.4ms) so the higher tick just means wheel events
        batch less and scrolling renders more continuously. *)
     fun () -> Bonsai_term.start ~target_frames_per_second:120. Gitter.App.app)
;;

let () = Command_unix.run command
