open! Core
module Vt = Gitter.Term_pane.Vt

let check name cond = if not cond then failwithf "FAILED: %s" name ()

let make ?(rows = 5) ?(cols = 10) () =
  let responses = Queue.create () in
  let publishes = ref 0 in
  let t =
    Vt.create
      ~rows
      ~cols
      ~respond:(Queue.enqueue responses)
      ~publish:(fun () -> incr publishes)
  in
  t, responses, publishes
;;

(* Plain text, wrapping, control chars. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "hello";
  check "text lands" (String.equal (Vt.row_text t 0) "hello");
  check "cursor advanced" ([%equal: int * int] (Vt.cursor t) (0, 5));
  Vt.feed_string t "world!!";
  check "wrap to next row" (String.equal (Vt.row_text t 1) "!!");
  let t, _, _ = make ~cols:5 () in
  Vt.feed_string t "abcde";
  check "wrap is deferred at last col" ([%equal: int * int] (Vt.cursor t) (0, 4));
  Vt.feed_string t "f";
  check "deferred wrap fires" (String.equal (Vt.row_text t 1) "f");
  let t, _, _ = make () in
  Vt.feed_string t "ab\rc";
  check "CR returns to col 0" (String.equal (Vt.row_text t 0) "cb")
;;

(* Cursor addressing and clears. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "\x1b[2;3HX";
  check "CUP is 1-based" (String.equal (Vt.row_text t 1) "  X");
  Vt.feed_string t "\x1b[2;1Hab\x1b[1K";
  check "EL1 clears through cursor" (String.equal (Vt.row_text t 1) "");
  let t, _, _ = make () in
  Vt.feed_string t "one\x1b[2Htwo\x1b[3Hthree\x1b[2;2H\x1b[J";
  check "ED0 clears below" (String.equal (Vt.row_text t 2) "");
  check "ED0 keeps above" (String.equal (Vt.row_text t 0) "one");
  check "ED0 keeps left of cursor" (String.equal (Vt.row_text t 1) "t")
;;

(* SGR: truecolor, 256, reverse, BCE. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "\x1b[38;2;1;2;3m\x1b[48;5;196mA";
  let s = Vt.style_at t ~r:0 ~c:0 in
  check "truecolor fg" (Vt.Style.Color.equal s.fg (Rgb (1, 2, 3)));
  check "256 bg" (Vt.Style.Color.equal s.bg (Idx 196));
  Vt.feed_string t "\x1b[0m\x1b[7mB";
  check "reverse" (Vt.style_at t ~r:0 ~c:1).reverse;
  let t, _, _ = make () in
  Vt.feed_string t "\x1b[48;2;9;9;9m\x1b[2J";
  check
    "BCE: clear paints current bg"
    (Vt.Style.Color.equal (Vt.style_at t ~r:4 ~c:9).bg (Rgb (9, 9, 9)))
;;

(* Scroll region: LF at region bottom scrolls only the region. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "AAA\x1b[2HBBB\x1b[3HCCC\x1b[4HDDD\x1b[5HEEE";
  Vt.feed_string t "\x1b[2;4r"; (* region rows 2-4 *)
  Vt.feed_string t "\x1b[4;1H\n"; (* LF at region bottom *)
  check "region scrolled" (String.equal (Vt.row_text t 1) "CCC");
  check "line below region untouched" (String.equal (Vt.row_text t 4) "EEE");
  check "line above region untouched" (String.equal (Vt.row_text t 0) "AAA");
  check "blank enters at region bottom" (String.equal (Vt.row_text t 3) "")
;;

(* IL/DL inside the region. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "AAA\x1b[2HBBB\x1b[3HCCC";
  Vt.feed_string t "\x1b[2H\x1b[L";
  check "IL pushes lines down" (String.equal (Vt.row_text t 2) "BBB");
  check "IL blanks the cursor line" (String.equal (Vt.row_text t 1) "");
  Vt.feed_string t "\x1b[2H\x1b[M";
  check "DL pulls lines up" (String.equal (Vt.row_text t 1) "BBB")
;;

(* ICH/DCH/ECH. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "abcdef\x1b[1;2H\x1b[2@";
  check "ICH shifts right" (String.equal (Vt.row_text t 0) "a  bcdef");
  Vt.feed_string t "\x1b[1;1H\x1b[3P";
  check "DCH shifts left" (String.equal (Vt.row_text t 0) "bcdef");
  Vt.feed_string t "\x1b[1;1H\x1b[2X";
  check "ECH blanks in place" (String.equal (Vt.row_text t 0) "  def")
;;

(* Alt screen: enter clears, leave restores content and cursor. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "main line";
  Vt.feed_string t "\x1b[?1049h";
  check "alt starts blank" (String.equal (Vt.row_text t 0) "");
  Vt.feed_string t "alt stuff";
  Vt.feed_string t "\x1b[?1049l";
  check "main restored" (String.equal (Vt.row_text t 0) "main line");
  check "cursor restored" ([%equal: int * int] (Vt.cursor t) (0, 9))
;;

(* Synchronized output: publishes held until ESU. *)
let () =
  let t, _, publishes = make () in
  Vt.feed_string t "x";
  let base = !publishes in
  Vt.feed_string t "\x1b[?2026h";
  Vt.feed_string t "chunk one";
  Vt.feed_string t "chunk two";
  check "no publish inside sync block" (!publishes = base);
  Vt.feed_string t "\x1b[?2026l";
  check "one publish at ESU" (!publishes = base + 1);
  check "content applied" (String.is_prefix (Vt.row_text t 0) ~prefix:"xchunk one")
;;

(* Feed-chunk coalescing: many writes in one chunk = one publish. *)
let () =
  let t, _, publishes = make () in
  Vt.feed_string t "abc\x1b[2Hdef\x1b[3Hghi";
  check "one chunk -> one publish" (!publishes = 1)
;;

(* Identification queries editors block on. *)
let () =
  let t, responses, _ = make () in
  Vt.feed_string t "\x1b[c";
  check "primary DA answered" (String.is_prefix (Queue.dequeue_exn responses) ~prefix:"\x1b[?");
  Vt.feed_string t "\x1b[>c";
  check "secondary DA answered" (String.is_prefix (Queue.dequeue_exn responses) ~prefix:"\x1b[>");
  Vt.feed_string t "\x1b[3;4H\x1b[6n";
  check "CPR reports position" (String.equal (Queue.dequeue_exn responses) "\x1b[3;4R");
  Vt.feed_string t "\x1b]11;?\x07";
  check "OSC 11 answered" (String.is_prefix (Queue.dequeue_exn responses) ~prefix:"\x1b]11;rgb:")
;;

(* Modes the input encoder reads. *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "\x1b[?1000h\x1b[?1006h";
  check "mouse normal" (Vt.Mouse_mode.equal (Vt.mouse_mode t) Normal);
  check "sgr mouse" (Vt.mouse_sgr t);
  Vt.feed_string t "\x1b[?1002h";
  check "mouse button" (Vt.Mouse_mode.equal (Vt.mouse_mode t) Button);
  Vt.feed_string t "\x1b[?1000l";
  check "mouse off" (Vt.Mouse_mode.equal (Vt.mouse_mode t) Off);
  Vt.feed_string t "\x1b[?1h";
  check "app cursor keys" (Vt.app_cursor_keys t);
  Vt.feed_string t "\x1b[?25l";
  check "cursor hidden" (not (Vt.cursor_visible t))
;;

(* UTF-8 straddling a feed boundary. *)
let () =
  let t, _, _ = make () in
  let e_acute = "\xc3\xa9" in
  Vt.feed_string t "\xc3";
  Vt.feed_string t "\xa9x";
  check "split utf8 char assembles" (String.equal (Vt.row_text t 0) (e_acute ^ "x"))
;;

(* DEC line drawing (vim borders). *)
let () =
  let t, _, _ = make () in
  Vt.feed_string t "\x1b(0qqx\x1b(Bq";
  check "linedraw maps then restores" (String.equal (Vt.row_text t 0) "\u{2500}\u{2500}\u{2502}q")
;;

(* Resize preserves content. *)
let () =
  let t, _, _ = make ~rows:3 ~cols:5 () in
  Vt.feed_string t "abc";
  Vt.resize t ~rows:4 ~cols:8;
  check "resize keeps text" (String.equal (Vt.row_text t 0) "abc")
;;

let () = print_endline "All vt tests passed."
