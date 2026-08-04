open! Core
open! Bonsai_term

(* The command tree: declarative data, which-key style. Every entry is a
   mnemonic key plus either an effect to run or a submenu. The menu renders
   itself from this structure and interprets keys against it, so adding a
   capability anywhere in the app is just contributing entries — never new
   key-routing code. Effects typically close over a layer's control handle
   (e.g. [Layout.Component.Controls]). *)

type t =
  | Action of
      { key : char
      ; label : string
      ; effect : unit Effect.t
      }
  | Group of
      { key : char
      ; label : string
      ; children : t list
      }

let key = function
  | Action { key; _ } | Group { key; _ } -> key
;;

let label = function
  | Action { label; _ } | Group { label; _ } -> label
;;

let find entries c = List.find entries ~f:(fun entry -> Char.equal (key entry) c)

(* The entries visible after pressing the keys in [path], if the path still
   resolves to a group. *)
let rec at_path entries path =
  match path with
  | [] -> Some entries
  | c :: rest ->
    (match find entries c with
     | Some (Group { children; _ }) -> at_path children rest
     | Some (Action _) | None -> None)
;;

(* The popup title for a path: the group's label, helix-style. *)
let rec title_at entries path =
  match path with
  | [] -> "Space"
  | c :: rest ->
    (match find entries c with
     | Some (Group { label; children; _ }) ->
       if List.is_empty rest then String.capitalize label else title_at children rest
     | Some (Action _) | None -> "Space")
;;

(* The flat projection for the command palette: every action with the key
   path that reaches it. *)
module Flat = struct
  type t =
    { keys : char list
    ; label : string
    ; effect : unit Effect.t
    }

  let bindings t =
    "space " ^ String.concat ~sep:" " (List.map t.keys ~f:Char.to_string)
  ;;
end

let rec flatten ?(path = []) entries =
  List.concat_map entries ~f:(function
    | Action { key; label; effect } -> [ { Flat.keys = path @ [ key ]; label; effect } ]
    | Group { key; children; _ } -> flatten ~path:(path @ [ key ]) children)
;;
