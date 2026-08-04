open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* Rows of UI above the embedded terminal pane; keep in sync with the header
   views in [app]. *)
let header_rows = 4

let app ~(dimensions : Dimensions.t Bonsai.t) (local_ graph)
  : view:View.t Bonsai.t * handler:(Event.t -> unit Effect.t) Bonsai.t
  =
  let count, inject_count =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ctx count action ->
        match action with
        | `Incr -> count + 1
        | `Decr -> count - 1)
      graph
  in
  let focus, toggle_focus =
    Bonsai.state_machine
      ~default_model:`Counter
      ~apply_action:(fun _ctx focus `Toggle ->
        match focus with
        | `Counter -> `Terminal
        | `Terminal -> `Counter)
      graph
  in
  let term_dimensions =
    let%arr dimensions in
    { Dimensions.height = Int.max 1 (dimensions.height - header_rows)
    ; width = dimensions.width
    }
  in
  let tmux =
    Bonsai_term_tmux.component
      ~command:(Bonsai.return "zsh")
      ~dimensions:term_dimensions
      graph
  in
  let view =
    let%arr count and focus and tmux in
    let tab name ~active =
      if active
      then View.text ~attrs:[ Attr.bold; Attr.invert ] (" " ^ name ^ " ")
      else View.text (" " ^ name ^ " ")
    in
    let terminal_view =
      match tmux.last_view with
      | Pending -> View.text "starting terminal..."
      | Error e -> View.text (Error.to_string_hum e)
      | Ok v -> v
    in
    View.vcat
      [ View.text ~attrs:[ Attr.bold ] "gitter - bonsai_term demo"
      ; View.hcat
          [ tab "counter" ~active:(match focus with `Counter -> true | `Terminal -> false)
          ; tab "terminal" ~active:(match focus with `Terminal -> true | `Counter -> false)
          ; View.text "  (Ctrl-T switches focus, Ctrl-C quits)"
          ]
      ; View.text (sprintf "Counter: %d (Up/Down arrows when focused)" count)
      ; View.text ""
      ; terminal_view
      ]
  in
  let handler =
    let%arr focus and inject_count and toggle_focus and tmux in
    fun (event : Event.t) ->
      match event with
      | Event.Key_press { key = ASCII ('t' | 'T'); mods = [ Ctrl ] } ->
        toggle_focus `Toggle
      | event ->
        (match focus with
         | `Terminal -> tmux.handler event
         | `Counter ->
           (match event with
            | Event.Key_press { key = Arrow `Up; _ } -> inject_count `Incr
            | Event.Key_press { key = Arrow `Down; _ } -> inject_count `Decr
            | _ -> Effect.Ignore))
  in
  ~view, ~handler
;;

let command =
  Async.Command.async_or_error
    ~summary:"bonsai_term demo with an embedded terminal"
    (let%map_open.Command () = return () in
     fun () -> Bonsai_term.start app)
;;

let () = Command_unix.run command
