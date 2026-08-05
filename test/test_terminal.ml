open! Core
module Vterm = Gitter.Terminal.Vterm

(* The libvterm wrapper: feeding, rendering, and — the part we rely on the
   library for — input encoding that honors the modes the app negotiated. *)

let check name cond = if cond then printf "PASS %s\n" name else printf "FAIL %s\n" name

let check_s name ~expect actual =
  if String.equal expect actual
  then printf "PASS %s\n" name
  else printf "FAIL %s: expected %S got %S\n" name expect actual
;;

let fresh () = Vterm.create ~rows:6 ~cols:20

let () =
  (* plain text *)
  let t = fresh () in
  Vterm.feed_string t "hello";
  check_s "plain text" ~expect:"hello" (Vterm.row_text t 0);
  (* cursor addressing *)
  let t = fresh () in
  Vterm.feed_string t "\x1b[3;5Hmid";
  check_s "cursor addressing" ~expect:"    mid" (Vterm.row_text t 2);
  let r, c, visible = Vterm.cursor t in
  check "cursor position" (r = 2 && c = 7 && visible);
  (* SGR colors reach the packed cells; indexed colors stay SYMBOLIC so
     the host terminal resolves them with the user's palette *)
  let t = fresh () in
  Vterm.feed_string t "\x1b[1m\x1b[38;2;255;0;0mR\x1b[0m";
  let `Attrs attrs, `Fg fg, `Bg bg = Vterm.cell_style t ~r:0 ~c:0 in
  check "bold bit" (attrs land 1 <> 0);
  check "truecolor fg stays rgb" (match fg with `Rgb (255, 0, 0) -> true | _ -> false);
  check "default bg" (match bg with `Default -> true | _ -> false);
  let t = fresh () in
  Vterm.feed_string t "\x1b[31mR\x1b[94mB\x1b[0m";
  let _, `Fg red, _ = Vterm.cell_style t ~r:0 ~c:0 in
  let _, `Fg bright_blue, _ = Vterm.cell_style t ~r:0 ~c:1 in
  check "ansi fg passes through as index" (match red with `Indexed 1 -> true | _ -> false);
  check
    "bright ansi fg passes through"
    (match bright_blue with `Indexed 12 -> true | _ -> false);
  let t = fresh () in
  Vterm.feed_string t "\x1b[48;5;196mX\x1b[0m";
  let _, _, `Bg bg = Vterm.cell_style t ~r:0 ~c:0 in
  check "256-color bg passes through" (match bg with `Indexed 196 -> true | _ -> false);
  (* erase + rewrite (the tear-prone path under snapshots) *)
  let t = fresh () in
  Vterm.feed_string t "aaaaa\r\x1b[Kbbb";
  check_s "erase to eol" ~expect:"bbb" (Vterm.row_text t 0);
  (* scroll region *)
  let t = fresh () in
  Vterm.feed_string t "\x1b[1;3rL1\r\nL2\r\nL3\r\nL4";
  check_s "scroll region keeps last row" ~expect:"L2" (Vterm.row_text t 0);
  (* wide char: leading cell carries the glyph, continuation is skipped *)
  let t = fresh () in
  Vterm.feed_string t "\xe4\xbd\xa0x";
  check_s "wide char" ~expect:"\xe4\xbd\xa0x" (Vterm.row_text t 0);
  let r, c, _ = Vterm.cursor t in
  check "wide char advances 2" (r = 0 && c = 3);
  (* alt screen switch and return *)
  let t = fresh () in
  Vterm.feed_string t "main\x1b[?1049h\x1b[2J\x1b[Halt";
  check_s "alt screen" ~expect:"alt" (Vterm.row_text t 0);
  Vterm.feed_string t "\x1b[?1049l";
  check_s "primary restored" ~expect:"main" (Vterm.row_text t 0);
  (* device attributes query produces a response in the output buffer *)
  let t = fresh () in
  let (_ : string) = Vterm.take_output t in
  Vterm.feed_string t "\x1b[c";
  let out = Vterm.take_output t in
  check "DA response" (String.is_prefix out ~prefix:"\x1b[?");
  (* damage flag: set by feeding, cleared by take *)
  let t = fresh () in
  let (_ : bool) = Vterm.take_damage t in
  Vterm.feed_string t "x";
  check "damage set" (Vterm.take_damage t);
  check "damage cleared" (not (Vterm.take_damage t));
  (* key encoding: plain, ctrl, and arrows in both cursor-key modes *)
  let t = fresh () in
  let (_ : string) = Vterm.take_output t in
  Vterm.send_key t (ASCII 'a') [];
  check_s "key a" ~expect:"a" (Vterm.take_output t);
  Vterm.send_key t (ASCII 'c') [ Ctrl ];
  check_s "ctrl-c" ~expect:"\x03" (Vterm.take_output t);
  (* notty delivers Ctrl+letter as UPPERCASE — must still encode 0x03,
     not libvterm's CSIu (ESC[67;5u), which plain shells print as junk *)
  Vterm.send_key t (ASCII 'C') [ Ctrl ];
  check_s "ctrl-c (uppercase, the notty form)" ~expect:"\x03" (Vterm.take_output t);
  Vterm.send_key t (ASCII 'R') [ Ctrl ];
  check_s "ctrl-r (uppercase)" ~expect:"\x12" (Vterm.take_output t);
  Vterm.send_key t (Arrow `Up) [];
  check_s "arrow (normal mode)" ~expect:"\x1b[A" (Vterm.take_output t);
  Vterm.feed_string t "\x1b[?1h";
  let (_ : string) = Vterm.take_output t in
  Vterm.send_key t (Arrow `Up) [];
  check_s "arrow (application mode)" ~expect:"\x1bOA" (Vterm.take_output t);
  Vterm.send_key t Enter [];
  check_s "enter" ~expect:"\r" (Vterm.take_output t);
  (* mouse: silent with no mode; SGR-encoded once the app asks (1006) *)
  let t = fresh () in
  let (_ : string) = Vterm.take_output t in
  Vterm.send_mouse t Left ~row:2 ~col:3 ~mods:[];
  check_s "mouse silent without mode" ~expect:"" (Vterm.take_output t);
  (* new terminal: the modeless press above left button 1 down in state *)
  let t = fresh () in
  Vterm.feed_string t "\x1b[?1000h\x1b[?1006h";
  let (_ : string) = Vterm.take_output t in
  Vterm.send_mouse t Left ~row:2 ~col:3 ~mods:[];
  let out = Vterm.take_output t in
  check_s "mouse SGR press" ~expect:"\x1b[<0;4;3M" out;
  Vterm.send_mouse t Release ~row:2 ~col:3 ~mods:[];
  let out = Vterm.take_output t in
  check_s "mouse SGR release" ~expect:"\x1b[<0;4;3m" out;
  Vterm.send_mouse t (Scroll `Down) ~row:2 ~col:3 ~mods:[];
  let out = Vterm.take_output t in
  check "wheel encodes" (String.is_substring out ~substring:"\x1b[<6");
  (* resize *)
  let t = fresh () in
  Vterm.feed_string t "wrap-me";
  Vterm.resize t ~rows:6 ~cols:40;
  check_s "survives resize" ~expect:"wrap-me" (Vterm.row_text t 0);
  (* render smoke test: full grid renders at the right dimensions *)
  let t = fresh () in
  Vterm.feed_string t "\x1b[7mrev\x1b[0m plain";
  let view = Vterm.render t in
  check
    "render dimensions"
    (Bonsai_term.View.height view = 6 && Bonsai_term.View.width view = 20)
;;
