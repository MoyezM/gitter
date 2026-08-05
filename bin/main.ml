open! Core
open Async

(* Plain [Bonsai_term.start]: it owns Ctrl-C exit (notty reports it as
   uppercase 'C' — matching it by hand is how it silently broke once).
   The archived embedded terminal needed [start_with_driver] for the
   [Gitter.Wake] frame-loop wakeup; reviving it means switching back —
   see lib/terminal/README.md. *)
let run () =
  (* The whole app treats paths as repo-root-relative (git status
     porcelain semantics), so anchor there no matter where gitter was
     launched — also what makes `gitter` work from a subdirectory. Not a
     git repo: leave cwd alone and let the status pane show the error. *)
  let%bind.Deferred () =
    match%map.Deferred
      Process.run ~prog:"git" ~args:[ "rev-parse"; "--show-toplevel" ] ()
    with
    | Error (_ : Error.t) -> ()
    | Ok out ->
      (match String.strip out with
       | "" -> ()
       | root ->
         (try Core_unix.chdir root with
          | _ -> ()))
  in
  (* 120 covers ProMotion displays; frames are cheap (windowed highlight
     queries run 0.1–0.4ms) so the higher tick just means wheel events
     batch less and scrolling renders more continuously. *)
  Bonsai_term.start ~target_frames_per_second:120. Gitter.App.app
;;

let command =
  Async.Command.async_or_error
    ~summary:"gitter"
    (let%map_open.Command () = return () in
     run)
;;

let () = Command_unix.run command
