open! Core
open Async
open! Bonsai_term

(* The pty side of the embedded terminal: spawns [$EDITOR file] on a
   forkpty'd pty (C stub — the OCaml runtime never forks), reads master-fd
   bytes straight into the [Vt] emulator, and writes encoded input bytes
   back. No subprocesses after spawn, no snapshots: the Vt publishes
   coalesced, tear-free screen generations. *)

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
  { vt : Vt.t
  ; master : Fd.t
  ; writer : Writer.t
  ; child : Pid.t
  ; mutable closed : bool
  ; on_closed : unit -> unit
  }

let is_closed t = t.closed
let vt t = t.vt

let reader_loop t reader =
  let buf = Bytes.create 65536 in
  let rec loop () =
    match%bind Reader.read reader buf with
    | `Eof ->
      t.closed <- true;
      t.on_closed ();
      return ()
    | `Ok len ->
      Vt.feed t.vt buf ~len;
      loop ()
  in
  loop ()
;;

let create ~command ~cwd ~rows ~cols ~publish ~on_closed =
  let vt_ref = ref None in
  let pid, master_fd = pty_spawn rows cols cwd "xterm-256color" command in
  let fd =
    Fd.create
      Fifo
      (Core_unix.File_descr.of_int master_fd)
      (Info.of_string "gitter pty master")
  in
  let writer = Writer.create fd in
  let vt =
    Vt.create
      ~rows
      ~cols
      ~respond:(fun s -> Writer.write writer s)
      ~publish
  in
  vt_ref := Some vt;
  let t =
    { vt
    ; master = fd
    ; writer
    ; child = Pid.of_int pid
    ; closed = false
    ; on_closed
    }
  in
  don't_wait_for (reader_loop t (Reader.create fd));
  t
;;

(* ---- input encoding ---------------------------------------------------- *)

let ctrl c = String.of_char (Char.of_int_exn (Char.to_int (Char.lowercase c) land 0x1f))

let key_bytes t (key : Event.Key.t) (mods : Event.Modifier.t list) =
  let has m = List.mem mods m ~equal:Event.Modifier.equal in
  let meta s = if has Event.Modifier.Meta then Some ("\x1b" ^ s) else Some s in
  let arrow suffix =
    (* application cursor keys mode flips CSI to SS3 *)
    if Vt.app_cursor_keys t.vt then meta ("\x1bO" ^ suffix) else meta ("\x1b[" ^ suffix)
  in
  match key with
  | Event.Key.ASCII c when has Event.Modifier.Ctrl -> meta (ctrl c)
  | ASCII c -> meta (String.of_char c)
  | Uchar u ->
    let b = Stdlib.Buffer.create 4 in
    Stdlib.Buffer.add_utf_8_uchar b (Stdlib.Uchar.of_int (Uchar.to_scalar u));
    meta (Stdlib.Buffer.contents b)
  | Enter -> meta "\r"
  | Escape -> Some "\x1b"
  | Tab -> if has Event.Modifier.Shift then Some "\x1b[Z" else meta "\t"
  | Backspace -> meta "\x7f"
  | Insert -> meta "\x1b[2~"
  | Delete -> meta "\x1b[3~"
  | Home -> arrow "H"
  | End -> arrow "F"
  | Arrow `Up -> arrow "A"
  | Arrow `Down -> arrow "B"
  | Arrow `Right -> arrow "C"
  | Arrow `Left -> arrow "D"
  | Page `Up -> meta "\x1b[5~"
  | Page `Down -> meta "\x1b[6~"
  | Function n when n <= 4 -> Some (sprintf "\x1bO%c" "PQRS".[n - 1])
  | Function n ->
    let code =
      match n with
      | 5 -> 15 | 6 -> 17 | 7 -> 18 | 8 -> 19 | 9 -> 20 | 10 -> 21 | 11 -> 23 | _ -> 24
    in
    Some (sprintf "\x1b[%d~" code)
;;

(* SGR (1006) mouse encoding at pane-local coordinates. *)
let mouse_bytes t (kind : Event.mouse_kind) ~x ~y ~(mods : Event.Modifier.t list) =
  let mode = Vt.mouse_mode t.vt in
  let wanted =
    match kind, mode with
    | _, Vt.Mouse_mode.Off -> false
    | (Left | Middle | Right | Release | Scroll _), (Normal | Button | Any) -> true
    | Drag, (Button | Any) -> true
    | Hover, Any -> true
    | Drag, Normal | Hover, (Normal | Button) -> false
  in
  if not (wanted && Vt.mouse_sgr t.vt)
  then None
  else (
    let base =
      match kind with
      | Left -> 0
      | Middle -> 1
      | Right -> 2
      | Release -> 0
      | Drag -> 32 (* motion with left held *)
      | Hover -> 35 (* motion, no button *)
      | Scroll `Up -> 64
      | Scroll `Down -> 65
    in
    let base =
      base
      + (if List.mem mods Event.Modifier.Shift ~equal:Event.Modifier.equal then 4 else 0)
      + (if List.mem mods Event.Modifier.Meta ~equal:Event.Modifier.equal then 8 else 0)
      + if List.mem mods Event.Modifier.Ctrl ~equal:Event.Modifier.equal then 16 else 0
    in
    let final = match kind with Release -> 'm' | _ -> 'M' in
    Some (sprintf "\x1b[<%d;%d;%d%c" base (x + 1) (y + 1) final))
;;

(* [position] must already be pane-local (the component subtracts its border
   offset). *)
let send_event t (event : Event.t) =
  if not t.closed
  then (
    match event with
    | Event.Key_press { key; mods } ->
      Option.iter (key_bytes t key mods) ~f:(Writer.write t.writer)
    | Mouse { kind; position; mods } ->
      (match mouse_bytes t kind ~x:position.x ~y:position.y ~mods with
       | Some s -> Writer.write t.writer s
       | None ->
         (* app didn't enable the mouse: translate the wheel to arrows so
            scrolling still works in plain editors *)
         (match kind with
          | Scroll `Up -> Option.iter (key_bytes t (Arrow `Up) []) ~f:(Writer.write t.writer)
          | Scroll `Down ->
            Option.iter (key_bytes t (Arrow `Down) []) ~f:(Writer.write t.writer)
          | _ -> ()))
    | Paste _ -> ())
;;

let resize t ~rows ~cols =
  if not t.closed
  then (
    Vt.resize t.vt ~rows ~cols;
    (* TIOCSWINSZ delivers SIGWINCH to the child *)
    try pty_set_winsize (Core_unix.File_descr.to_int (Fd.file_descr_exn t.master)) rows cols with
    | _ -> ())
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    Signal_unix.send_i Signal.hup (`Pid t.child);
    don't_wait_for
      (let%bind () = Writer.close t.writer ~force_close:(Clock_ns.after (Time_ns.Span.of_sec 1.)) in
       Fd.close t.master))
;;
