open! Core
open! Bonsai_term

(* Catppuccin Mocha-ish palette. *)
let rgb = Attr.Color.rgb
let text = rgb ~r:205 ~g:214 ~b:244
let dim = rgb ~r:108 ~g:112 ~b:134
let blue = rgb ~r:137 ~g:180 ~b:250
let surface = rgb ~r:49 ~g:50 ~b:68
let popup = rgb ~r:17 ~g:17 ~b:27 (* near-black popup fill, helix-style *)

let header = [ Attr.fg blue; Attr.bold ]
let context = [ Attr.fg dim ]
let border = [ Attr.fg dim ]
let border_focused = [ Attr.fg blue ]
let status_bar = [ Attr.fg text; Attr.bg surface ]
