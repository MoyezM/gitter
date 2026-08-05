open! Core
open Async

(* [start_with_driver]: the driver handle lets background producers (the
   shell overlay's pty reader) wake the frame loop via [Gitter.Wake].
   Ctrl-C exit lives in [App.exit_on_ctrlc], BELOW the shell overlay so
   a visible terminal captures Ctrl-C as SIGINT — the app owns the
   layering, so the library's [make_app_exit_on_ctrlc] (outermost, and
   the keeper of the uppercase-'C' notty lore) no longer fits. *)
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
  let%bind.Deferred.Or_error driver =
    Bonsai_term.start_with_driver
      (* 120 covers ProMotion displays; frames are cheap (windowed
         highlight queries run 0.1–0.4ms) so the higher tick just means
         wheel events batch less and scrolling renders more
         continuously. *)
      ~target_frames_per_second:120.
      ~get_view_and_handler:Fn.id
      ~handle_incoming:(fun _ () -> Bonsai_term.Effect.Ignore)
      (fun ~exit ~dimensions (local_ graph) ->
        Bonsai_term.stitch (Gitter.App.app ~exit ~dimensions graph))
  in
  Gitter.Wake.set (fun () -> Bonsai_term.Driver.send_incoming_event driver ());
  match%map.Deferred Bonsai_term.Driver.finished driver with
  | Ok () | Error `Incoming_events_pipe_closed -> Ok ()
;;

let command =
  Async.Command.async_or_error
    ~summary:"gitter"
    (let%map_open.Command () = return () in
     run)
;;

let () = Command_unix.run command
