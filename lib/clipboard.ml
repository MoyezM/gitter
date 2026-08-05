open! Core
open Async

(* Cross-platform copy-to-clipboard. Preferred path: a native tool on PATH
   (pbcopy on macOS, wl-copy/xclip/xsel on Linux), spawned exactly like
   Git.Runner spawns git. When none exists the caller falls back to OSC 52
   — a terminal escape that asks the HOSTING terminal to set the clipboard.
   It needs no binary and works over SSH, but terminal support varies
   (Terminal.app ignores it), so it is the fallback rather than the
   default. *)

let which prog =
  Sys.getenv "PATH"
  |> Option.value ~default:""
  |> String.split ~on:':'
  |> List.find_map ~f:(fun dir ->
    if String.is_empty dir
    then None
    else (
      let path = Filename.concat dir prog in
      match Core_unix.access path [ `Exec ] with
      | Ok () -> Some path
      | Error _ -> None))
;;

(* wl-copy talks to a Wayland compositor, xclip/xsel to an X server —
   without the corresponding display there is no clipboard to set, so
   don't pick a tool that can only fail. *)
let candidates () =
  let have v = Option.is_some (Sys.getenv v) in
  [ "pbcopy", [], true
  ; "wl-copy", [], have "WAYLAND_DISPLAY"
  ; "xclip", [ "-selection"; "clipboard" ], have "DISPLAY"
  ; "xsel", [ "--clipboard"; "--input" ], have "DISPLAY"
  ]
  |> List.filter_map ~f:(fun (prog, args, usable) ->
    if usable then which prog |> Option.map ~f:(fun path -> path, args) else None)
;;

let copy_via_tool text =
  match List.hd (candidates ()) with
  | None -> return (Or_error.error_string "no clipboard tool on PATH")
  | Some (prog, args) ->
    (match%map Process.run ~prog ~args ~stdin:text () with
     | Ok (_ : string) -> Ok ()
     | Error e -> Error (Error.tag_s e ~tag:[%sexp "clipboard", (prog : string)]))
;;

let base64 s =
  let tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let n = String.length s in
  let buf = Stdlib.Buffer.create (4 * ((n + 2) / 3)) in
  let byte i = if i < n then Char.to_int s.[i] else 0 in
  let rec go i =
    if i < n
    then (
      let b0 = byte i
      and b1 = byte (i + 1)
      and b2 = byte (i + 2) in
      Stdlib.Buffer.add_char buf tbl.[b0 lsr 2];
      Stdlib.Buffer.add_char buf tbl.[(b0 land 3) lsl 4 lor (b1 lsr 4)];
      Stdlib.Buffer.add_char
        buf
        (if i + 1 < n then tbl.[(b1 land 15) lsl 2 lor (b2 lsr 6)] else '=');
      Stdlib.Buffer.add_char buf (if i + 2 < n then tbl.[b2 land 63] else '=');
      go (i + 3))
  in
  go 0;
  Stdlib.Buffer.contents buf
;;

let osc52 text = sprintf "\x1b]52;c;%s\x07" (base64 text)
