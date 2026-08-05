open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The embedded terminal: a fullscreen overlay hosting [$EDITOR <file>],
   toggleable to the background (Ctrl-T) while the process keeps running —
   the tmux session stays alive, only its polling pauses. Opening a file
   while an editor is still running just foregrounds the existing session:
   we never inject keys into a running editor. *)

module Model = struct
  type t =
    { visible : bool
    ; command : string option (* None = nothing spawned yet *)
    }

  let initial = { visible = false; command = None }
end

let editor () =
  match Stdlib.Sys.getenv_opt "EDITOR" with
  | Some e when not (String.is_empty (String.strip e)) -> e
  | _ -> "vi"
;;

type t =
  { model : Model.t Bonsai.t
  ; set : (Model.t -> unit Effect.t) Bonsai.t
  ; term : Bonsai_term_tmux.t Bonsai.t
  }

let create ~(dimensions : Dimensions.t Bonsai.t) (local_ graph) =
  let model, set = Bonsai.state Model.initial graph in
  (* The working dir is pinned to where gitter was launched — paths from
     the status tree are relative to it. *)
  let cwd = Stdlib.Sys.getcwd () in
  let term =
    Bonsai_term_tmux.component
      ~persistence:Keep_command_alive_if_component_deactivates
      ~mouse:Classic
      ~working_dir:(Bonsai.return (Some cwd))
      ~active:
        (let%arr model in
         model.visible)
      ~command:
        (let%arr model in
         (* "true" is the unspawned sentinel: exits immediately, never shown
            (visible only becomes true alongside a real command). *)
         Option.value model.command ~default:"true")
      ~dimensions:
        (let%arr dimensions in
         { Dimensions.height = Int.max 1 (dimensions.height - 2)
         ; width = Int.max 1 (dimensions.width - 2)
         })
      graph
  in
  { model; set; term }
;;

module Controls = struct
  type t =
    { open_file : string -> unit Effect.t
    ; toggle : unit Effect.t
    }
end

let controls { model; set; term } =
  let%arr model and set and term in
  let running = (not term.Bonsai_term_tmux.is_closed) && Option.is_some model.command in
  { Controls.open_file =
      (fun path ->
        let command = sprintf "%s %s" (editor ()) (Filename.quote path) in
        if running
        then (* an editor is live: foreground it, don't disturb it *)
          set { model with Model.visible = true }
        else if [%equal: string option] model.command (Some command)
        then
          (* same file again after the editor exited: same command string, so
             the component won't respawn on its own — ask it to *)
          Effect.Many
            [ set { Model.visible = true; command = Some command }
            ; term.Bonsai_term_tmux.reinstantiate_command
            ]
        else set { Model.visible = true; command = Some command })
  ; toggle =
      (match model.command with
       | None -> Effect.Ignore
       | Some _ -> set { model with Model.visible = not model.visible })
  }
;;

let wrap { model; set; term } (base : Widget.t) : Widget.t =
  fun ~dimensions (local_ graph) ->
  let ~view:base_view, ~handler:base_handler = base ~dimensions graph in
  let view =
    let%arr base_view and model and term and dimensions in
    if not model.visible
    then base_view
    else (
      let inner =
        match term.Bonsai_term_tmux.last_view with
        | Pending_or_error.Ok v -> v
        | Pending -> View.text ~attrs:Theme.context " starting terminal..."
        | Error e -> View.text ~attrs:Theme.untracked (" terminal error: " ^ Error.to_string_hum e)
      in
      let title =
        if term.Bonsai_term_tmux.is_closed
        then " editor exited — any key returns to gitter "
        else " editor — Ctrl-T: background "
      in
      let boxed =
        View.zcat
          [ View.pad ~l:2 (View.text ~attrs:Theme.header title)
          ; Bonsai_term_border_box.view ~line_type:Thin ~attrs:Theme.border_focused inner
          ]
      in
      View.crop
        ~r:(Int.max 0 (View.width boxed - dimensions.width))
        ~b:(Int.max 0 (View.height boxed - dimensions.height))
        boxed)
  in
  let handler =
    let%arr base_handler and model and set and term in
    fun (event : Event.t) ->
      match model.visible, event with
      (* Ctrl-T toggles both ways (foreground only once something ran). *)
      | true, Key_press { key = ASCII ('t' | 'T'); mods = [ Ctrl ] } ->
        set { model with Model.visible = false }
      | false, Key_press { key = ASCII ('t' | 'T'); mods = [ Ctrl ] } ->
        if Option.is_some model.command
        then set { model with Model.visible = true }
        else Effect.Ignore
      | false, event -> base_handler event
      | true, event ->
        if term.Bonsai_term_tmux.is_closed
        then (
          match event with
          | Key_press _ -> set { model with Model.visible = false }
          | Mouse _ | Paste _ -> Effect.Ignore)
        else term.Bonsai_term_tmux.handler event
  in
  ~view, ~handler
;;
