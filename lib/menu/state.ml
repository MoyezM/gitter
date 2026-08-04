open! Core

(* The menu is closed, open at a mnemonic path, or showing the command
   palette. *)

module Model = struct
  type t =
    | Closed
    | Menu of char list
    | Palette of
        { query : string
        ; cursor : int
        }
  [@@deriving sexp, equal]
end

module Action = struct
  type t =
    | Open_menu
    | Close
    | Descend of char
    | Open_palette
    | Query_append of char
    | Query_backspace
    | Set_cursor of int
  [@@deriving sexp_of]
end

let apply_action _ctx (model : Model.t) (action : Action.t) : Model.t =
  match action, model with
  | Open_menu, _ -> Menu []
  | Close, _ -> Closed
  | Open_palette, _ -> Palette { query = ""; cursor = 0 }
  | Descend c, Menu path -> Menu (path @ [ c ])
  | Descend _, m -> m
  | Query_append c, Palette { query; _ } ->
    Palette { query = query ^ Char.to_string c; cursor = 0 }
  | Query_backspace, Palette { query; _ } ->
    Palette { query = String.drop_suffix query 1; cursor = 0 }
  | Set_cursor cursor, Palette { query; _ } -> Palette { query; cursor }
  | (Query_append _ | Query_backspace | Set_cursor _), m -> m
;;
