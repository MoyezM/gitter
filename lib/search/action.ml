open! Core
open! Bonsai_term

type 'pane t =
  | Pane of 'pane
  | Prompt of
      { event : Prompt.event
      ; height : int
      }
  | Jump of
      { dir : int
      ; height : int
      }
  | By_mode of
      { if_active : 'pane t option
      ; if_idle : 'pane t option
      }
[@@deriving sexp_of]

let pane a = Pane a
let when_active a = By_mode { if_active = Some a; if_idle = None }
let when_idle a = By_mode { if_active = None; if_idle = Some a }
let both ~active ~idle = By_mode { if_active = Some active; if_idle = Some idle }
let jump ~height dir = Jump { dir; height }
let implicit_commit = Prompt { event = Prompt.Implicit_commit; height = 0 }

(* The leaf-surface pair every searchable pane exposes the same way
   (see [Widget.pane]): the prompt-open fact and the implicit commit. *)
let surface ~inject ~search =
  let open Bonsai.Let_syntax in
  ( (let%arr search in
     Prompt.is_active search)
  , (let%arr inject in
     inject implicit_commit) )
;;

let resolve ~search t =
  match t with
  | By_mode { if_active; if_idle } ->
    if Prompt.is_active search then if_active else if_idle
  | t -> Some t
;;

let keymap ~height ~idle_char ?enter_idle (event : Event.t) =
  let prompt event = Prompt { event; height } in
  (* While the prompt is active every printable appends — nothing is
     stolen; idle, the same key is its normal binding (or nothing). *)
  let typed ?if_idle text =
    By_mode { if_active = Some (prompt (Prompt.Type text)); if_idle }
  in
  match event with
  | Event.Key_press { key = ASCII '/'; mods = [] } ->
    Some (typed ~if_idle:(prompt Prompt.Open) "/")
  | Key_press { key = ASCII ('n' as c); mods = [] }
  | Key_press { key = ASCII ('N' as c); mods = [] } ->
    Some
      (typed
         ~if_idle:(Jump { dir = (if Char.equal c 'n' then 1 else -1); height })
         (Char.to_string c))
  | Key_press { key = ASCII c; mods = [] } ->
    Some (typed ?if_idle:(idle_char c) (Char.to_string c))
  | Key_press { key = Uchar u; mods = [] } -> Some (typed (Prompt.utf8 u))
  | Key_press { key = Enter; mods = [] } ->
    Some (By_mode { if_active = Some (prompt Commit); if_idle = enter_idle })
  | Key_press { key = Escape; mods = [] } ->
    Some (both ~active:(prompt Cancel) ~idle:(prompt Clear))
  | Key_press { key = Backspace; mods = [] } -> Some (when_active (prompt Backspace))
  | _ when Chord.ctrl 'p' event -> Some (when_active (prompt Recall_prev))
  | _ when Chord.ctrl 'n' event -> Some (when_active (prompt Recall_next))
  | _ -> None
;;
