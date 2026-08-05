open! Core
module Clipboard = Gitter.Clipboard

let check name cond = if not cond then failwithf "FAILED: %s" name ()

(* OSC 52 payloads are base64 (hand-rolled in clipboard.ml — no dep);
   cover every padding case. *)
let () =
  let osc b64 = sprintf "\x1b]52;c;%s\x07" b64 in
  check "len mod 3 = 0" (String.equal (Clipboard.osc52 "foo/ba") (osc "Zm9vL2Jh"));
  check "len mod 3 = 1" (String.equal (Clipboard.osc52 "lib/app.ml") (osc "bGliL2FwcC5tbA=="));
  check "len mod 3 = 2" (String.equal (Clipboard.osc52 "app.ts") (osc "YXBwLnRz"));
  check "empty" (String.equal (Clipboard.osc52 "") (osc ""));
  print_endline "All clipboard tests passed."
;;
