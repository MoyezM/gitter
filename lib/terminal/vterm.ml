open! Core
open! Bonsai_term

(* Safe wrapper over libvterm (lib/vterm_stubs.c): the known-good terminal
   emulator neovim embeds, replacing the hand-rolled Vt. libvterm owns ALL
   the semantics: parsing, grid state, and crucially the INPUT ENCODING —
   keys and mouse are encoded by the library itself, honoring whatever
   modes the application negotiated, so there is no mode tracking here. *)

type raw

external raw_new : int -> int -> raw = "gt_vterm_new"
external raw_feed : raw -> Bytes.t -> int -> unit = "gt_vterm_feed"
external raw_take_output : raw -> string = "gt_vterm_take_output"
external raw_take_damage : raw -> bool = "gt_vterm_take_damage"
external raw_resize : raw -> int -> int -> unit = "gt_vterm_resize"
external raw_key_unichar : raw -> int -> int -> unit = "gt_vterm_key_unichar"
external raw_key_special : raw -> int -> int -> unit = "gt_vterm_key_special"
external raw_mouse_move : raw -> int -> int -> int -> unit = "gt_vterm_mouse_move"
external raw_mouse_button : raw -> int -> bool -> int -> unit = "gt_vterm_mouse_button"
external raw_cursor : raw -> int * int * bool = "gt_vterm_cursor"
external raw_read_row : raw -> int -> int -> Bytes.t -> unit = "gt_vterm_read_row"

type t =
  { raw : raw
  ; mutable rows : int
  ; mutable cols : int
  ; mutable row_buf : Bytes.t (* one row of 20-byte packed cells *)
  }

let cell_bytes = 20

let create ~rows ~cols =
  { raw = raw_new rows cols; rows; cols; row_buf = Bytes.create (cols * cell_bytes) }
;;

let feed t bytes ~len = raw_feed t.raw bytes len
let take_output t = raw_take_output t.raw
let take_damage t = raw_take_damage t.raw

let resize t ~rows ~cols =
  if rows <> t.rows || cols <> t.cols
  then (
    raw_resize t.raw rows cols;
    t.rows <- rows;
    t.cols <- cols;
    t.row_buf <- Bytes.create (cols * cell_bytes))
;;

let cursor t = raw_cursor t.raw

(* ---- input ------------------------------------------------------------- *)

(* VTermModifier bits *)
let mod_bits (mods : Event.Modifier.t list) =
  List.fold mods ~init:0 ~f:(fun acc m ->
    acc
    lor
    match m with
    | Event.Modifier.Shift -> 1
    | Meta -> 2
    | Ctrl -> 4)
;;

(* Key codes for gt_vterm_key_special — keep in sync with the C switch. *)
let special_code (key : Event.Key.t) =
  match key with
  | Enter -> Some 0
  | Tab -> Some 1
  | Backspace -> Some 2
  | Escape -> Some 3
  | Arrow `Up -> Some 4
  | Arrow `Down -> Some 5
  | Arrow `Left -> Some 6
  | Arrow `Right -> Some 7
  | Insert -> Some 8
  | Delete -> Some 9
  | Home -> Some 10
  | End -> Some 11
  | Page `Up -> Some 12
  | Page `Down -> Some 13
  | Function n -> Some (100 + n)
  | ASCII _ | Uchar _ -> None
;;

let send_key t (key : Event.Key.t) (mods : Event.Modifier.t list) =
  let m = mod_bits mods in
  match special_code key with
  | Some code -> raw_key_special t.raw code m
  | None ->
    (match key with
     | ASCII c -> raw_key_unichar t.raw (Char.to_int c) m
     | Uchar u -> raw_key_unichar t.raw (Uchar.to_scalar u) m
     | _ -> ())
;;

(* libvterm buttons: 1 left, 2 middle, 3 right, 4 wheel-up, 5 wheel-down.
   It encodes only if the app enabled a mouse mode — callers can always
   forward. Returns whether the event could produce encoding (wheel
   fallthrough decisions belong to the caller via [take_output]). *)
let send_mouse t (kind : Event.mouse_kind) ~row ~col ~(mods : Event.Modifier.t list) =
  let m = mod_bits mods in
  raw_mouse_move t.raw row col m;
  match kind with
  | Left -> raw_mouse_button t.raw 1 true m
  | Middle -> raw_mouse_button t.raw 2 true m
  | Right -> raw_mouse_button t.raw 3 true m
  | Release ->
    (* release of whichever button; left covers the common case *)
    raw_mouse_button t.raw 1 false m
  | Scroll `Up -> raw_mouse_button t.raw 4 true m
  | Scroll `Down -> raw_mouse_button t.raw 5 true m
  | Drag | Hover -> () (* the move above is the event *)
;;

(* ---- rendering --------------------------------------------------------- *)

let attr_cache : (string, Attr.t list) Hashtbl.t = Hashtbl.create (module String)

let attrs_of_packed style_key =
  Hashtbl.find_or_add attr_cache style_key ~default:(fun () ->
    let b = style_key in
    let bits = Char.to_int b.[0] in
    let color which tag r g bl =
      if Char.to_int b.[tag] = 0
      then None
      else
        Some
          (which
             (Attr.Color.rgb
                ~r:(Char.to_int b.[r])
                ~g:(Char.to_int b.[g])
                ~b:(Char.to_int b.[bl])))
    in
    List.filter_opt
      [ color Attr.fg 1 2 3 4
      ; color Attr.bg 5 6 7 8
      ; Option.some_if (bits land 1 <> 0) Attr.bold
      ; Option.some_if (bits land 2 <> 0) Attr.italic
      ; Option.some_if (bits land 4 <> 0) Attr.underline
      ; Option.some_if (bits land 8 <> 0) Attr.invert
      ])
;;

(* One row: decode packed cells, merge consecutive same-style runs. The
   9-byte style key is sliced straight out of the packed buffer. *)
let render_row t ~cursor_r ~cursor_c ~cursor_visible r =
  raw_read_row t.raw r t.cols t.row_buf;
  let buf = t.row_buf in
  let segs = ref [] in
  let text = Stdlib.Buffer.create t.cols in
  let cur_style = ref "" in
  let flush () =
    if Stdlib.Buffer.length text > 0
    then (
      segs := View.text ~attrs:(attrs_of_packed !cur_style) (Stdlib.Buffer.contents text) :: !segs;
      Stdlib.Buffer.clear text)
  in
  for c = 0 to t.cols - 1 do
    let base = c * cell_bytes in
    let len = Char.to_int (Bytes.get buf base) in
    if len > 0
    then (
      let style =
        let key = Bytes.To_string.sub buf ~pos:(base + 9) ~len:9 in
        if cursor_visible && r = cursor_r && c = cursor_c
        then (
          (* cursor cell: flip the reverse bit *)
          let bits = Char.to_int key.[0] lxor 8 in
          String.of_char (Char.of_int_exn bits) ^ String.drop_prefix key 1)
        else key
      in
      if not (String.equal style !cur_style)
      then (
        flush ();
        cur_style := style);
      Stdlib.Buffer.add_string text (Bytes.To_string.sub buf ~pos:(base + 1) ~len))
  done;
  flush ();
  View.hcat (List.rev !segs)
;;

let render t =
  let cursor_r, cursor_c, cursor_visible = cursor t in
  View.vcat (List.init t.rows ~f:(render_row t ~cursor_r ~cursor_c ~cursor_visible))
;;

(* ---- test helpers ------------------------------------------------------ *)

let feed_string t s = feed t (Bytes.of_string s) ~len:(String.length s)

let row_text t r =
  raw_read_row t.raw r t.cols t.row_buf;
  let buf = Stdlib.Buffer.create t.cols in
  for c = 0 to t.cols - 1 do
    let base = c * cell_bytes in
    let len = Char.to_int (Bytes.get t.row_buf base) in
    if len > 0 then Stdlib.Buffer.add_string buf (Bytes.To_string.sub t.row_buf ~pos:(base + 1) ~len)
  done;
  String.rstrip (Stdlib.Buffer.contents buf)
;;

let cell_style t ~r ~c =
  raw_read_row t.raw r t.cols t.row_buf;
  let base = c * cell_bytes in
  let byte i = Char.to_int (Bytes.get t.row_buf (base + i)) in
  let color tag rr gg bb =
    if byte tag = 0 then None else Some (byte rr, byte gg, byte bb)
  in
  ( `Attrs (byte 9)
  , `Fg (color 10 11 12 13)
  , `Bg (color 14 15 16 17) )
;;
