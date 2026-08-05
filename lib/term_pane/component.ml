open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The embedded terminal: a fullscreen overlay hosting [$EDITOR <file>],
   toggleable to the background (Ctrl-T) while the process keeps running.
   Backed by [Terminal.Session]: a real pty feeding libvterm —
   keystrokes are synchronous fd writes, screens are tear-free grid
   generations, and the mouse passes through SGR-encoded. While an editor
   is live, opening another file just foregrounds the existing session: we
   never inject keys into a running editor. *)

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
  ; generation : int Bonsai.t (* Vt screen generation + lifecycle events *)
  ; generation_var : int Bonsai.Expert.Var.t
  ; client : Terminal.Session.t option ref
  ; dimensions : Dimensions.t Bonsai.t
  ; cwd : string
  }

(* rows/cols inside the border box *)
let inner (dimensions : Dimensions.t) =
  Int.max 1 (dimensions.height - 2), Int.max 1 (dimensions.width - 2)
;;

let create ~(dimensions : Dimensions.t Bonsai.t) (local_ graph) =
  let model, set = Bonsai.state Model.initial graph in
  let generation_var = Bonsai.Expert.Var.create 0 in
  let client = ref None in
  (* The working dir is pinned to where gitter was launched — paths from
     the status tree are relative to it. *)
  let cwd = Stdlib.Sys.getcwd () in
  (* Follow pane resizes while a session is live. *)
  let size =
    let%arr dimensions in
    inner dimensions
  in
  let resize_callback =
    Bonsai.return (fun (rows, cols) ->
      Effect.of_thunk (fun () ->
        Option.iter !client ~f:(fun c -> Terminal.Session.resize c ~rows ~cols)))
  in
  Bonsai.Edge.on_change ~equal:[%equal: int * int] ~callback:resize_callback size graph;
  { model
  ; set
  ; generation = Bonsai.Expert.Var.value generation_var
  ; generation_var
  ; client
  ; dimensions
  ; cwd
  }
;;

module Controls = struct
  type t =
    { open_file : string -> unit Effect.t
    ; toggle : unit Effect.t
    }
end

let controls t =
  let%arr model = t.model
  and set = t.set
  and generation = t.generation
  and dimensions = t.dimensions in
  ignore (generation : int);
  let running =
    match !(t.client) with
    | Some c -> not (Terminal.Session.is_closed c)
    | None -> false
  in
  let bump () =
    Bonsai.Expert.Var.update t.generation_var ~f:(fun g -> g + 1);
    Wake.wake ()
  in
  let spawn command =
    Effect.of_thunk (fun () ->
      Option.iter !(t.client) ~f:Terminal.Session.close;
      let rows, cols = inner dimensions in
      t.client
      := Some
           (Terminal.Session.create
              ~command
              ~cwd:t.cwd
              ~rows
              ~cols
              ~publish:bump
              ~on_closed:bump);
      bump ())
  in
  { Controls.open_file =
      (fun path ->
        let command = sprintf "%s %s" (editor ()) (Filename.quote path) in
        if running
        then (* an editor is live: foreground it, don't disturb it *)
          set { model with Model.visible = true }
        else
          Effect.Many
            [ set { Model.visible = true; command = Some command }; spawn command ])
  ; toggle =
      (match model.Model.command with
       | None -> Effect.Ignore
       | Some _ -> set { model with Model.visible = not model.Model.visible })
  }
;;

let wrap t (base : Widget.t) : Widget.t =
  fun ~dimensions (local_ graph) ->
  let ~view:base_view, ~handler:base_handler = base ~dimensions graph in
  let view =
    let%arr base_view
    and model = t.model
    (* named and consumed: an ignored pattern here gets PRUNED from the
       dependency set by ppx_bonsai, and screen updates published by the
       pty reader stop repainting (they only show up on the next
       unrelated event). Same in [controls]. *)
    and generation = t.generation
    and dimensions in
    ignore (generation : int);
    if not model.Model.visible
    then base_view
    else (
      let closed =
        match !(t.client) with
        | Some c -> Terminal.Session.is_closed c
        | None -> true
      in
      let inner_view =
        match !(t.client) with
        | Some c -> Terminal.Session.render c
        | None -> View.text ~attrs:Theme.context " starting editor..."
      in
      let title =
        if closed
        then " editor exited — any key returns to gitter "
        else " editor — Ctrl-T: background "
      in
      let boxed =
        View.zcat
          [ View.pad ~l:2 (View.text ~attrs:Theme.header title)
          ; Bonsai_term_border_box.view ~line_type:Thin ~attrs:Theme.border_focused inner_view
          ]
      in
      View.crop
        ~r:(Int.max 0 (View.width boxed - dimensions.width))
        ~b:(Int.max 0 (View.height boxed - dimensions.height))
        boxed)
  in
  (* No generation dependency here: the closure reads [t.client] at
     event time, so it never goes stale. *)
  let handler =
    let%arr base_handler
    and model = t.model
    and set = t.set
    and dimensions in
    fun (event : Event.t) ->
      match model.Model.visible, event with
      (* Ctrl-T toggles both ways (foreground only once something ran). *)
      | true, Key_press { key = ASCII ('t' | 'T'); mods = [ Ctrl ] } ->
        set { model with Model.visible = false }
      | false, Key_press { key = ASCII ('t' | 'T'); mods = [ Ctrl ] } ->
        if Option.is_some model.Model.command
        then set { model with Model.visible = true }
        else Effect.Ignore
      | false, event -> base_handler event
      | true, event ->
        (match !(t.client) with
         | Some c when not (Terminal.Session.is_closed c) ->
           (* synchronous fd write — no deferred hop on the key path *)
           Effect.of_thunk (fun () ->
             match event with
             | Event.Mouse { kind; position; mods } ->
               (* translate to pane-local (inside the border) and drop
                  border clicks *)
               let x = position.x - 1
               and y = position.y - 1 in
               let rows, cols = inner dimensions in
               if x >= 0 && y >= 0 && x < cols && y < rows
               then
                 Terminal.Session.send_event
                   c
                   (Event.Mouse { kind; position = { Position.x; y }; mods })
             | event -> Terminal.Session.send_event c event)
         | Some _ | None ->
           (match event with
            | Key_press _ -> set { model with Model.visible = false }
            | Mouse _ | Paste _ -> Effect.Ignore))
  in
  ~view, ~handler
;;
