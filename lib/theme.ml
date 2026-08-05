open! Core
open! Bonsai_term

(* Catppuccin Mocha-ish palette. *)
let rgb = Attr.Color.rgb
let text = rgb ~r:205 ~g:214 ~b:244
let dim = rgb ~r:108 ~g:112 ~b:134
let blue = rgb ~r:137 ~g:180 ~b:250
let surface = rgb ~r:49 ~g:50 ~b:68
let popup = rgb ~r:17 ~g:17 ~b:27 (* near-black popup fill, helix-style *)

let green = rgb ~r:166 ~g:227 ~b:161
let red = rgb ~r:243 ~g:139 ~b:168
let yellow = rgb ~r:249 ~g:226 ~b:175
let mauve = rgb ~r:203 ~g:166 ~b:247
let peach = rgb ~r:250 ~g:179 ~b:135
let teal = rgb ~r:148 ~g:226 ~b:213
let header = [ Attr.fg blue; Attr.bold ]
let context = [ Attr.fg dim ]
let selection_bg = Attr.bg surface
let staged = [ Attr.fg green ]
let unstaged = [ Attr.fg yellow ]
let untracked = [ Attr.fg red ]
(* Diff row tints: muted backgrounds with normal text, delta-style. Enough
   chroma to keep their hue against a translucent terminal background. *)
let added_bg = rgb ~r:36 ~g:62 ~b:45
let removed_bg = rgb ~r:76 ~g:38 ~b:50
let added_bar = [ Attr.fg green; Attr.bg added_bg ]
let removed_bar = [ Attr.fg red; Attr.bg removed_bg ]

(* Capture-name -> foreground for syntax highlighting. Matches on the part
   before any '.', so "string.special" styles like "string". [None] means
   the row's default foreground. *)
let syntax capture =
  let group = String.lsplit2 capture ~on:'.' |> Option.value_map ~default:capture ~f:fst in
  match group with
  | "comment" -> Some [ Attr.fg dim; Attr.italic ]
  | "string" -> Some [ Attr.fg green ]
  | "keyword" -> Some [ Attr.fg mauve ]
  | "type" | "constructor" | "module" -> Some [ Attr.fg yellow ]
  | "number" | "constant" | "boolean" -> Some [ Attr.fg peach ]
  | "function" | "method" -> Some [ Attr.fg blue ]
  | "attribute" | "label" -> Some [ Attr.fg teal ]
  | "operator" | "punctuation" -> Some [ Attr.fg dim ]
  | _ -> None
;;
let border = [ Attr.fg dim ]
let border_focused = [ Attr.fg blue ]
let status_bar = [ Attr.fg text; Attr.bg surface ]
