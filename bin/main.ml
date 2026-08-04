open! Core

let command =
  Async.Command.async_or_error
    ~summary:"gitter"
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start Gitter.App.app)
;;

let () = Command_unix.run command
