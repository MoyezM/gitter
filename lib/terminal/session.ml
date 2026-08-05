open! Core
open Async
open! Bonsai_term

(* A live embedded-terminal session: a command on a forkpty'd pty (C stub —
   the OCaml runtime never forks) wired to a libvterm emulator. Generic:
   the editor overlay uses it today; any pane wanting an embedded TUI can
   host one.

   The pty master is deliberately NOT wrapped in an Async Fd: macOS kqueue
   cannot watch pty fds, so Reader/Writer wake up for the first operation
   and then hang forever. Instead reads are blocking read(2) calls on a
   pool thread ([In_thread.run]) and writes are synchronous write(2) —
   which is also what makes the keystroke path zero-hop. *)

external pty_spawn
  :  int
  -> int
  -> string
  -> string
  -> string
  -> int * int
  = "gitter_pty_spawn"

external pty_set_winsize : int -> int -> int -> unit = "gitter_pty_set_winsize"

type t =
  { vterm : Vterm.t
  ; master : Core_unix.File_descr.t
  ; child : Pid.t
  ; mutable closed : bool
  ; publish : unit -> unit
  ; on_closed : unit -> unit
  }

let is_closed t = t.closed
let vterm t = t.vterm

(* Synchronous write of anything libvterm wants to say to the application
   (query responses, encoded input). A few bytes into a kernel pty buffer:
   effectively instant while the child is alive. *)
let write_pty t s =
  if (not t.closed) && not (String.is_empty s)
  then (
    let len = String.length s in
    let rec go pos =
      if pos < len
      then (
        match Core_unix.single_write_substring t.master ~buf:s ~pos ~len:(len - pos) with
        | n -> go (pos + n)
        | exception Core_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> go pos
        | exception _ -> ())
    in
    go 0)
;;

let pump_output t = write_pty t (Vterm.take_output t.vterm)

let mark_closed t =
  if not t.closed
  then (
    t.closed <- true;
    (try Core_unix.close t.master with
     | _ -> ());
    t.on_closed ())
;;

(* Blocking reads on a pool thread; vterm feeding happens back on the Async
   thread (the same thread bonsai effects run on — no locking needed). *)
let reader_loop t =
  let buf = Bytes.create 65536 in
  let rec loop () =
    let%bind res =
      In_thread.run (fun () ->
        let rec go () =
          match Core_unix.read t.master ~buf ~pos:0 ~len:(Bytes.length buf) with
          | n -> `Ok n
          | exception Core_unix.Unix_error (EINTR, _, _) -> go ()
          (* EIO/EBADF: the child side is gone *)
          | exception _ -> `Eof
        in
        go ())
    in
    match res with
    | `Eof | `Ok 0 ->
      mark_closed t;
      return ()
    | `Ok len ->
      Vterm.feed t.vterm buf ~len;
      pump_output t;
      if Vterm.take_damage t.vterm then t.publish ();
      loop ()
  in
  loop ()
;;

let create ~command ~cwd ~rows ~cols ~publish ~on_closed =
  let pid, master_fd = pty_spawn rows cols cwd "xterm-256color" command in
  let t =
    { vterm = Vterm.create ~rows ~cols
    ; master = Core_unix.File_descr.of_int master_fd
    ; child = Pid.of_int pid
    ; closed = false
    ; publish
    ; on_closed
    }
  in
  don't_wait_for (reader_loop t);
  t
;;

let send_event t (event : Event.t) =
  if not t.closed
  then (
    match event with
    | Event.Key_press { key; mods } ->
      Vterm.send_key t.vterm key mods;
      pump_output t
    | Mouse { kind; position; mods } ->
      Vterm.send_mouse t.vterm kind ~row:position.y ~col:position.x ~mods;
      let out = Vterm.take_output t.vterm in
      if not (String.is_empty out)
      then write_pty t out
      else (
        (* app without a mouse mode: keep the wheel useful *)
        (match kind with
         | Scroll `Up -> Vterm.send_key t.vterm (Arrow `Up) []
         | Scroll `Down -> Vterm.send_key t.vterm (Arrow `Down) []
         | _ -> ());
        pump_output t)
    | Paste _ -> ())
;;

let resize t ~rows ~cols =
  if not t.closed
  then (
    Vterm.resize t.vterm ~rows ~cols;
    (* TIOCSWINSZ delivers SIGWINCH to the child *)
    (try pty_set_winsize (Core_unix.File_descr.to_int t.master) rows cols with
     | _ -> ());
    t.publish ())
;;

(* SIGHUP the child; the reader sees EOF when it exits and closes the fd.
   The fd is not closed here — a pool thread may be blocked in read(2) on
   it, and the EOF path is the safe place to release it. *)
let close t = if not t.closed then Signal_unix.send_i Signal.hup (`Pid t.child)
let render t = Vterm.render t.vterm
