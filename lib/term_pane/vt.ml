open! Core
open! Bonsai_term

(* A VT/xterm terminal emulator sized for full-screen editors: a mutable
   cell grid fed by raw pty bytes. This exists because SNAPSHOT approaches
   (tmux capture-pane, in any transport) sample mid-redraw — tearing,
   waterfalling, flashes — and can't do first-class mouse. Feeding the byte
   stream statefully makes torn frames impossible: synchronized-output
   (DECSET 2026) batches are held and published atomically, and everything
   else publishes only between feed chunks.

   Coverage is the editor repertoire: cursor addressing, erase/insert/
   delete, scroll regions, alt screen, SGR through truecolor, DEC modes
   (cursor visibility, autowrap, app cursor keys, bracketed paste, mouse
   incl. SGR encoding), DEC line-drawing charset, and the identification
   queries editors block on (DA, CPR, OSC 10/11). *)

module Style = struct
  module Color = struct
    type t =
      | Default
      | Idx of int
      | Rgb of int * int * int
    [@@deriving equal, sexp_of]
  end

  type t =
    { fg : Color.t
    ; bg : Color.t
    ; bold : bool
    ; dim : bool
    ; italic : bool
    ; underline : bool
    ; reverse : bool
    }
  [@@deriving equal, sexp_of]

  let default =
    { fg = Default; bg = Default; bold = false; dim = false; italic = false
    ; underline = false; reverse = false
    }
  ;;
end

module Cell = struct
  type t =
    { ch : string (* one utf-8 grapheme; " " when empty *)
    ; style : Style.t
    }
  [@@deriving equal, sexp_of]

  let blank ~style = { ch = " "; style = { style with Style.fg = Default; bold = false; dim = false; italic = false; underline = false; reverse = false } }
end

module Mouse_mode = struct
  type t =
    | Off
    | Normal (* 1000: press/release/wheel *)
    | Button (* 1002: + drag *)
    | Any (* 1003: + motion *)
  [@@deriving equal, sexp_of]
end

type grid = Cell.t array array

type parse_state =
  | Ground
  | Esc
  | Csi of { buf : Stdlib.Buffer.t }
  | Osc of { buf : Stdlib.Buffer.t; esc : bool }
  | Charset of char

type t =
  { mutable rows : int
  ; mutable cols : int
  ; mutable grid : grid (* the ACTIVE grid *)
  ; mutable saved_main : grid option (* main grid while on the alt screen *)
  ; mutable cur_r : int
  ; mutable cur_c : int
  ; mutable style : Style.t
  ; mutable saved_cursor : (int * int * Style.t) option
  ; mutable top : int (* scroll region, 0-based inclusive *)
  ; mutable bot : int
  ; mutable wrap_pending : bool
  ; mutable autowrap : bool
  ; mutable cursor_visible : bool
  ; mutable mouse : Mouse_mode.t
  ; mutable mouse_sgr : bool
  ; mutable app_cursor_keys : bool
  ; mutable bracketed_paste : bool [@warning "-69"] (* tracked for the encoder later *)
  ; mutable in_sync : bool
  ; mutable linedraw : bool
  ; mutable state : parse_state
  ; utf8_pending : Stdlib.Buffer.t
  ; mutable utf8_need : int
  ; mutable seq : int
  ; mutable dirty : bool
  ; respond : string -> unit (* answers to identification queries *)
  ; publish : unit -> unit
  }

let make_grid ~rows ~cols ~style =
  Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> Cell.blank ~style))
;;

let create ~rows ~cols ~respond ~publish =
  { rows
  ; cols
  ; grid = make_grid ~rows ~cols ~style:Style.default
  ; saved_main = None
  ; cur_r = 0
  ; cur_c = 0
  ; style = Style.default
  ; saved_cursor = None
  ; top = 0
  ; bot = rows - 1
  ; wrap_pending = false
  ; autowrap = true
  ; cursor_visible = true
  ; mouse = Mouse_mode.Off
  ; mouse_sgr = false
  ; app_cursor_keys = false
  ; bracketed_paste = false
  ; in_sync = false
  ; linedraw = false
  ; state = Ground
  ; utf8_pending = Stdlib.Buffer.create 4
  ; utf8_need = 0
  ; seq = 0
  ; dirty = false
  ; respond
  ; publish
  }
;;

(* ---- grid primitives --------------------------------------------------- *)

let blank_row t = Array.init t.cols ~f:(fun _ -> Cell.blank ~style:t.style)
let clamp_r t r = Int.clamp_exn r ~min:0 ~max:(t.rows - 1)
let clamp_c t c = Int.clamp_exn c ~min:0 ~max:(t.cols - 1)

let clear_cells t row ~from ~until =
  let row = t.grid.(row) in
  for c = Int.max 0 from to Int.min (t.cols - 1) until do
    row.(c) <- Cell.blank ~style:t.style
  done
;;

(* Scroll the region [top..bot] up by n (contents move up, blanks at the
   bottom) — the hot path for editor scrolling. *)
let scroll_up t n =
  let n = Int.min n (t.bot - t.top + 1) in
  for r = t.top to t.bot - n do
    t.grid.(r) <- t.grid.(r + n)
  done;
  for r = t.bot - n + 1 to t.bot do
    t.grid.(r) <- blank_row t
  done
;;

let scroll_down t n =
  let n = Int.min n (t.bot - t.top + 1) in
  for r = t.bot downto t.top + n do
    t.grid.(r) <- t.grid.(r - n)
  done;
  for r = t.top to t.top + n - 1 do
    t.grid.(r) <- blank_row t
  done
;;

let linefeed t =
  t.wrap_pending <- false;
  if t.cur_r = t.bot then scroll_up t 1 else t.cur_r <- clamp_r t (t.cur_r + 1)
;;

let reverse_linefeed t =
  t.wrap_pending <- false;
  if t.cur_r = t.top then scroll_down t 1 else t.cur_r <- clamp_r t (t.cur_r - 1)
;;

(* DEC special graphics: the line-drawing charset vim uses for borders. *)
let linedraw_map = function
  | "q" -> "\u{2500}" | "x" -> "\u{2502}" | "l" -> "\u{250C}" | "k" -> "\u{2510}"
  | "m" -> "\u{2514}" | "j" -> "\u{2518}" | "t" -> "\u{251C}" | "u" -> "\u{2524}"
  | "w" -> "\u{252C}" | "v" -> "\u{2534}" | "n" -> "\u{253C}" | "a" -> "\u{2592}"
  | "~" -> "\u{00B7}" | s -> s
;;

let put_char t ch =
  let ch = if t.linedraw then linedraw_map ch else ch in
  if t.wrap_pending && t.autowrap
  then (
    t.wrap_pending <- false;
    t.cur_c <- 0;
    linefeed t);
  t.grid.(t.cur_r).(t.cur_c) <- { Cell.ch; style = t.style };
  if t.cur_c = t.cols - 1 then t.wrap_pending <- true else t.cur_c <- t.cur_c + 1;
  t.dirty <- true
;;

(* ---- publishing -------------------------------------------------------- *)

let maybe_publish t =
  if t.dirty && not t.in_sync
  then (
    t.dirty <- false;
    t.seq <- t.seq + 1;
    t.publish ())
;;

(* ---- CSI dispatch ------------------------------------------------------ *)

let params_of buf =
  let s = Stdlib.Buffer.contents buf in
  let body = String.drop_prefix s (if String.is_prefix s ~prefix:"?" then 1 else 0) in
  List.map (String.split body ~on:';') ~f:(fun p ->
    if String.is_empty p then None else Int.of_string_opt (String.take_while p ~f:Char.is_digit))
;;

let p0 ps ~default = match ps with Some n :: _ -> n | _ -> default
let p1 ps ~default = match ps with _ :: Some n :: _ -> n | _ -> default

let sgr t params =
  (* full SGR walk incl. 38/48;5;n and 38/48;2;r;g;b *)
  let ps = match params with [] -> [ Some 0 ] | ps -> ps in
  let rec go = function
    | [] -> ()
    | p :: rest ->
      let n = Option.value p ~default:0 in
      (match n with
       | 0 -> t.style <- Style.default; go rest
       | 1 -> t.style <- { t.style with bold = true }; go rest
       | 2 -> t.style <- { t.style with dim = true }; go rest
       | 3 -> t.style <- { t.style with italic = true }; go rest
       | 4 -> t.style <- { t.style with underline = true }; go rest
       | 7 -> t.style <- { t.style with reverse = true }; go rest
       | 22 -> t.style <- { t.style with bold = false; dim = false }; go rest
       | 23 -> t.style <- { t.style with italic = false }; go rest
       | 24 -> t.style <- { t.style with underline = false }; go rest
       | 27 -> t.style <- { t.style with reverse = false }; go rest
       | 39 -> t.style <- { t.style with fg = Default }; go rest
       | 49 -> t.style <- { t.style with bg = Default }; go rest
       | n when n >= 30 && n <= 37 -> t.style <- { t.style with fg = Idx (n - 30) }; go rest
       | n when n >= 40 && n <= 47 -> t.style <- { t.style with bg = Idx (n - 40) }; go rest
       | n when n >= 90 && n <= 97 -> t.style <- { t.style with fg = Idx (n - 90 + 8) }; go rest
       | n when n >= 100 && n <= 107 -> t.style <- { t.style with bg = Idx (n - 100 + 8) }; go rest
       | 38 | 48 ->
         let set color = if n = 38 then t.style <- { t.style with fg = color } else t.style <- { t.style with bg = color } in
         (match rest with
          | Some 5 :: idx :: rest' -> set (Idx (Option.value idx ~default:0)); go rest'
          | Some 2 :: r :: g :: b :: rest' ->
            set (Rgb (Option.value r ~default:0, Option.value g ~default:0, Option.value b ~default:0));
            go rest'
          | _ -> ())
       | _ -> go rest)
  in
  go ps
;;

let dec_mode t ~set n =
  match n with
  | 1 -> t.app_cursor_keys <- set
  | 7 -> t.autowrap <- set
  | 12 -> () (* cursor blink *)
  | 25 -> t.cursor_visible <- set; t.dirty <- true
  | 1000 -> t.mouse <- (if set then Mouse_mode.Normal else Off)
  | 1002 -> t.mouse <- (if set then Mouse_mode.Button else Off)
  | 1003 -> t.mouse <- (if set then Mouse_mode.Any else Off)
  | 1005 | 1015 -> ()
  | 1006 -> t.mouse_sgr <- set
  | 2004 -> t.bracketed_paste <- set
  | 2026 ->
    t.in_sync <- set;
    if not set then maybe_publish t
  | 47 | 1047 | 1049 ->
    if set
    then (
      if Option.is_none t.saved_main
      then (
        t.saved_main <- Some t.grid;
        t.saved_cursor <- Some (t.cur_r, t.cur_c, t.style);
        t.grid <- make_grid ~rows:t.rows ~cols:t.cols ~style:Style.default;
        t.dirty <- true))
    else (
      match t.saved_main with
      | Some g ->
        t.grid <- g;
        t.saved_main <- None;
        (match t.saved_cursor with
         | Some (r, c, s) ->
           t.cur_r <- clamp_r t r;
           t.cur_c <- clamp_c t c;
           t.style <- s
         | None -> ());
        t.dirty <- true
      | None -> ())
  | _ -> ()
;;

let csi t buf final =
  let raw = Stdlib.Buffer.contents buf in
  let private_ = String.is_prefix raw ~prefix:"?" || String.is_prefix raw ~prefix:">" in
  let ps = params_of buf in
  t.wrap_pending <- false;
  match final, private_ with
  | 'h', true -> List.iter ps ~f:(Option.iter ~f:(dec_mode t ~set:true))
  | 'l', true -> List.iter ps ~f:(Option.iter ~f:(dec_mode t ~set:false))
  | 'c', _ ->
    (* editors block on device attributes *)
    if String.is_prefix raw ~prefix:">"
    then t.respond "\x1b[>0;0;0c" (* secondary DA *)
    else if String.is_prefix raw ~prefix:"?"
    then () (* tertiary etc. *)
    else t.respond "\x1b[?62;4c" (* primary DA *)
  | 'n', false ->
    if p0 ps ~default:0 = 6
    then t.respond (sprintf "\x1b[%d;%dR" (t.cur_r + 1) (t.cur_c + 1))
  | 'A', _ -> t.cur_r <- clamp_r t (t.cur_r - p0 ps ~default:1)
  | 'B', _ | 'e', _ -> t.cur_r <- clamp_r t (t.cur_r + p0 ps ~default:1)
  | 'C', _ | 'a', _ -> t.cur_c <- clamp_c t (t.cur_c + p0 ps ~default:1)
  | 'D', _ -> t.cur_c <- clamp_c t (t.cur_c - p0 ps ~default:1)
  | 'E', _ -> t.cur_r <- clamp_r t (t.cur_r + p0 ps ~default:1); t.cur_c <- 0
  | 'F', _ -> t.cur_r <- clamp_r t (t.cur_r - p0 ps ~default:1); t.cur_c <- 0
  | 'G', _ | '`', _ -> t.cur_c <- clamp_c t (p0 ps ~default:1 - 1)
  | 'd', _ -> t.cur_r <- clamp_r t (p0 ps ~default:1 - 1)
  | 'H', _ | 'f', _ ->
    t.cur_r <- clamp_r t (p0 ps ~default:1 - 1);
    t.cur_c <- clamp_c t (p1 ps ~default:1 - 1)
  | 'J', _ ->
    (match p0 ps ~default:0 with
     | 0 ->
       clear_cells t t.cur_r ~from:t.cur_c ~until:(t.cols - 1);
       for r = t.cur_r + 1 to t.rows - 1 do
         t.grid.(r) <- blank_row t
       done
     | 1 ->
       clear_cells t t.cur_r ~from:0 ~until:t.cur_c;
       for r = 0 to t.cur_r - 1 do
         t.grid.(r) <- blank_row t
       done
     | 2 | 3 ->
       for r = 0 to t.rows - 1 do
         t.grid.(r) <- blank_row t
       done
     | _ -> ());
    t.dirty <- true
  | 'K', _ ->
    (match p0 ps ~default:0 with
     | 0 -> clear_cells t t.cur_r ~from:t.cur_c ~until:(t.cols - 1)
     | 1 -> clear_cells t t.cur_r ~from:0 ~until:t.cur_c
     | 2 -> clear_cells t t.cur_r ~from:0 ~until:(t.cols - 1)
     | _ -> ());
    t.dirty <- true
  | 'L', _ ->
    (* insert lines at cursor within the region *)
    if t.cur_r >= t.top && t.cur_r <= t.bot
    then (
      let n = p0 ps ~default:1 in
      let saved_top = t.top in
      t.top <- t.cur_r;
      scroll_down t n;
      t.top <- saved_top;
      t.dirty <- true)
  | 'M', _ ->
    if t.cur_r >= t.top && t.cur_r <= t.bot
    then (
      let n = p0 ps ~default:1 in
      let saved_top = t.top in
      t.top <- t.cur_r;
      scroll_up t n;
      t.top <- saved_top;
      t.dirty <- true)
  | 'P', _ ->
    (* delete chars: shift the rest of the row left *)
    let n = Int.min (p0 ps ~default:1) (t.cols - t.cur_c) in
    let row = t.grid.(t.cur_r) in
    for c = t.cur_c to t.cols - 1 - n do
      row.(c) <- row.(c + n)
    done;
    clear_cells t t.cur_r ~from:(t.cols - n) ~until:(t.cols - 1);
    t.dirty <- true
  | '@', _ ->
    (* insert blanks: shift right *)
    let n = Int.min (p0 ps ~default:1) (t.cols - t.cur_c) in
    let row = t.grid.(t.cur_r) in
    for c = t.cols - 1 downto t.cur_c + n do
      row.(c) <- row.(c - n)
    done;
    clear_cells t t.cur_r ~from:t.cur_c ~until:(t.cur_c + n - 1);
    t.dirty <- true
  | 'X', _ ->
    let n = p0 ps ~default:1 in
    clear_cells t t.cur_r ~from:t.cur_c ~until:(t.cur_c + n - 1);
    t.dirty <- true
  | 'S', _ -> scroll_up t (p0 ps ~default:1); t.dirty <- true
  | 'T', _ -> scroll_down t (p0 ps ~default:1); t.dirty <- true
  | 'r', _ ->
    let top = p0 ps ~default:1 - 1 in
    let bot = p1 ps ~default:t.rows - 1 in
    if bot > top
    then (
      t.top <- clamp_r t top;
      t.bot <- clamp_r t bot;
      t.cur_r <- t.top;
      t.cur_c <- 0)
  | 'm', false -> sgr t ps
  | 's', _ -> t.saved_cursor <- Some (t.cur_r, t.cur_c, t.style)
  | 'u', _ ->
    (match t.saved_cursor with
     | Some (r, c, s) ->
       t.cur_r <- clamp_r t r;
       t.cur_c <- clamp_c t c;
       t.style <- s
     | None -> ())
  | 't', _ | 'm', true -> () (* window ops / private m: xterm modifyOtherKeys *)
  | _ -> ()
;;

let osc t buf =
  let s = Stdlib.Buffer.contents buf in
  (* color queries editors block on: answer with our theme-ish darks *)
  if String.is_prefix s ~prefix:"10;?"
  then t.respond "\x1b]10;rgb:cdcd/d6d6/f4f4\x1b\\"
  else if String.is_prefix s ~prefix:"11;?"
  then t.respond "\x1b]11;rgb:1e1e/1e1e/2e2e\x1b\\"
;;

(* ---- byte feed --------------------------------------------------------- *)

let feed_byte t byte =
  match t.state with
  | Ground ->
    (match byte with
     | '\x1b' -> t.state <- Esc
     | '\n' | '\x0b' | '\x0c' -> linefeed t; t.dirty <- true
     | '\r' -> t.cur_c <- 0; t.wrap_pending <- false
     | '\b' -> t.cur_c <- Int.max 0 (t.cur_c - 1); t.wrap_pending <- false
     | '\t' ->
       t.cur_c <- clamp_c t ((t.cur_c / 8 * 8) + 8);
       t.wrap_pending <- false
     | '\x07' | '\x00' | '\x0e' | '\x0f' -> ()
     | c when Char.to_int c < 0x20 -> ()
     | c when Char.to_int c < 0x80 -> put_char t (String.of_char c)
     | c ->
       (* utf-8 lead byte *)
       let n = Char.to_int c in
       let need = if n < 0xe0 then 1 else if n < 0xf0 then 2 else 3 in
       Stdlib.Buffer.clear t.utf8_pending;
       Stdlib.Buffer.add_char t.utf8_pending c;
       t.utf8_need <- need;
       if t.utf8_need = 0 then put_char t (String.of_char c))
  | Esc ->
    (match byte with
     | '[' -> t.state <- Csi { buf = Stdlib.Buffer.create 8 }
     | ']' -> t.state <- Osc { buf = Stdlib.Buffer.create 16; esc = false }
     | '(' | ')' -> t.state <- Charset byte
     | 'M' -> reverse_linefeed t; t.dirty <- true; t.state <- Ground
     | 'D' -> linefeed t; t.dirty <- true; t.state <- Ground
     | 'E' -> linefeed t; t.cur_c <- 0; t.dirty <- true; t.state <- Ground
     | '7' -> t.saved_cursor <- Some (t.cur_r, t.cur_c, t.style); t.state <- Ground
     | '8' ->
       (match t.saved_cursor with
        | Some (r, c, s) ->
          t.cur_r <- clamp_r t r;
          t.cur_c <- clamp_c t c;
          t.style <- s
        | None -> ());
       t.state <- Ground
     | 'c' ->
       (* full reset *)
       t.grid <- make_grid ~rows:t.rows ~cols:t.cols ~style:Style.default;
       t.style <- Style.default;
       t.cur_r <- 0;
       t.cur_c <- 0;
       t.top <- 0;
       t.bot <- t.rows - 1;
       t.dirty <- true;
       t.state <- Ground
     | '=' | '>' -> t.state <- Ground (* keypad modes *)
     | '\\' -> t.state <- Ground (* stray ST *)
     | _ -> t.state <- Ground)
  | Csi { buf } ->
    if Char.between byte ~low:'\x40' ~high:'\x7e'
    then (
      t.state <- Ground;
      csi t buf byte)
    else Stdlib.Buffer.add_char buf byte
  | Osc { buf; esc } ->
    (match byte, esc with
     | '\x07', _ ->
       t.state <- Ground;
       osc t buf
     | '\\', true ->
       t.state <- Ground;
       osc t buf
     | '\x1b', _ -> t.state <- Osc { buf; esc = true }
     | c, _ -> Stdlib.Buffer.add_char buf c; t.state <- Osc { buf; esc = false })
  | Charset which ->
    (match which with
     | '(' -> t.linedraw <- Char.equal byte '0'
     | _ -> ());
    t.state <- Ground
;;

let feed t bytes ~len =
  for i = 0 to len - 1 do
    let byte = Bytes.get bytes i in
    if t.utf8_need > 0 && (match t.state with Ground -> true | _ -> false)
    then (
      Stdlib.Buffer.add_char t.utf8_pending byte;
      t.utf8_need <- t.utf8_need - 1;
      if t.utf8_need = 0 then put_char t (Stdlib.Buffer.contents t.utf8_pending))
    else feed_byte t byte
  done;
  maybe_publish t
;;

let feed_string t s = feed t (Bytes.of_string s) ~len:(String.length s)

(* ---- resize ------------------------------------------------------------ *)

let resize t ~rows ~cols =
  if rows <> t.rows || cols <> t.cols
  then (
    let copy g =
      let fresh = make_grid ~rows ~cols ~style:Style.default in
      for r = 0 to Int.min rows (Array.length g) - 1 do
        for c = 0 to Int.min cols (Array.length g.(r)) - 1 do
          fresh.(r).(c) <- g.(r).(c)
        done
      done;
      fresh
    in
    t.grid <- copy t.grid;
    t.saved_main <- Option.map t.saved_main ~f:copy;
    t.rows <- rows;
    t.cols <- cols;
    t.top <- 0;
    t.bot <- rows - 1;
    t.cur_r <- clamp_r t t.cur_r;
    t.cur_c <- clamp_c t t.cur_c;
    t.dirty <- true;
    maybe_publish t)
;;

(* ---- rendering --------------------------------------------------------- *)

(* xterm 256-color palette -> rgb *)
let idx_rgb n =
  if n < 16
  then (
    let hex =
      [| 0x1e1e2e; 0xf38ba8; 0xa6e3a1; 0xf9e2af; 0x89b4fa; 0xcba6f7; 0x94e2d5; 0xbac2de
       ; 0x585b70; 0xf38ba8; 0xa6e3a1; 0xf9e2af; 0x89b4fa; 0xcba6f7; 0x94e2d5; 0xcdd6f4
      |].(n)
    in
    (hex lsr 16) land 0xff, (hex lsr 8) land 0xff, hex land 0xff)
  else if n < 232
  then (
    let n = n - 16 in
    let comp i = if i = 0 then 0 else (i * 40) + 55 in
    comp (n / 36), comp (n / 6 % 6), comp (n % 6))
  else (
    let v = ((n - 232) * 10) + 8 in
    v, v, v)
;;

let color_attr which (c : Style.Color.t) =
  match c with
  | Default -> None
  | Idx n ->
    let r, g, b = idx_rgb n in
    Some (which (Attr.Color.rgb ~r ~g ~b))
  | Rgb (r, g, b) -> Some (which (Attr.Color.rgb ~r ~g ~b))
;;

let attrs_of_style (s : Style.t) =
  let fg, bg = if s.reverse then s.bg, s.fg else s.fg, s.bg in
  (* reversed default sides need concrete colors to actually swap *)
  let fg = if s.reverse && Style.Color.equal fg Default then Style.Color.Idx 0 else fg in
  let bg = if s.reverse && Style.Color.equal bg Default then Style.Color.Idx 15 else bg in
  List.filter_opt
    [ color_attr Attr.fg fg
    ; color_attr Attr.bg bg
    ; Option.some_if s.bold Attr.bold
    ; Option.some_if s.italic Attr.italic
    ; Option.some_if s.underline Attr.underline
    ]
;;

(* The grid as a View: consecutive same-style cells merge into single text
   segments. The cursor renders as a reversed cell. *)
let render t =
  let cursor_style (s : Style.t) = { s with reverse = not s.reverse } in
  View.vcat
    (List.init t.rows ~f:(fun r ->
       let row = t.grid.(r) in
       let segs = ref [] in
       let buf = Stdlib.Buffer.create 64 in
       let cur = ref None in
       let flush () =
         match !cur with
         | Some style when Stdlib.Buffer.length buf > 0 ->
           segs := View.text ~attrs:(attrs_of_style style) (Stdlib.Buffer.contents buf) :: !segs;
           Stdlib.Buffer.clear buf
         | _ -> Stdlib.Buffer.clear buf
       in
       for c = 0 to t.cols - 1 do
         let cell = row.(c) in
         let style =
           if t.cursor_visible && r = t.cur_r && c = t.cur_c
           then cursor_style cell.style
           else cell.style
         in
         (match !cur with
          | Some prev when Style.equal prev style -> ()
          | _ ->
            flush ();
            cur := Some style);
         Stdlib.Buffer.add_string buf cell.ch
       done;
       flush ();
       View.hcat (List.rev !segs)))
;;

(* ---- state the input encoder needs ------------------------------------- *)

let mouse_mode t = t.mouse
let mouse_sgr t = t.mouse_sgr
let app_cursor_keys t = t.app_cursor_keys
let seq t = t.seq

(* ---- introspection (tests) --------------------------------------------- *)

let cursor t = t.cur_r, t.cur_c
let cursor_visible t = t.cursor_visible

let row_text t r =
  if r < 0 || r >= t.rows
  then ""
  else
    String.concat (Array.to_list (Array.map t.grid.(r) ~f:(fun c -> c.Cell.ch)))
    |> String.rstrip
;;

let style_at t ~r ~c = t.grid.(r).(c).Cell.style
