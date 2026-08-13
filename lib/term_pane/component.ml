open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* The shell overlay: a padded modal terminal ([Terminal.Session] running
   $SHELL in the repo root) for quick git/gt commands without leaving
   gitter. Ctrl-T toggles both ways; the shell keeps running while
   hidden; exiting the shell closes the overlay and the next open spawns
   a fresh one. Hiding (or shell exit) fires [on_hide] so the app
   refreshes git state immediately instead of riding the 2s poller. *)

let margin_x = 6
let margin_y = 2

(* $SHELL is unset under some launchers. /bin/zsh is the macOS default and
   is always there; most Linux distros do not ship it at all, and only
   /bin/sh is guaranteed — falling through to a missing shell would open the
   overlay onto a command that exits 127 immediately. *)
let shell () =
  match Stdlib.Sys.getenv_opt "SHELL" with
  | Some s when not (String.is_empty (String.strip s)) -> s
  | _ -> if Stdlib.Sys.file_exists "/bin/zsh" then "/bin/zsh" else "/bin/sh"
;;

type t =
  { visible : bool Bonsai.t
  ; set_visible : (bool -> unit Effect.t) Bonsai.t
  ; generation : int Bonsai.t
  ; generation_var : int Bonsai.Expert.Var.t
  ; client : Terminal.Session.t option ref
  ; dimensions : Dimensions.t Bonsai.t
  ; cwd : string
  ; on_show : unit Effect.t Bonsai.t
  ; on_hide : unit Effect.t Bonsai.t
  ; on_ctrl_c : unit Effect.t Bonsai.t
  }

(* Session dims inside the padding and the border. *)
let inner (dimensions : Dimensions.t) =
  ( Int.max 2 (dimensions.height - (2 * margin_y) - 2)
  , Int.max 10 (dimensions.width - (2 * margin_x) - 2) )
;;

let create ~(dimensions : Dimensions.t Bonsai.t) ~on_show ~on_hide ~on_ctrl_c (local_ graph) =
  let visible, set_visible = Bonsai.state false graph in
  let generation_var = Bonsai.Expert.Var.create 0 in
  let client = ref None in
  (* bin/main anchors the process at the repo root. *)
  let cwd = Stdlib.Sys.getcwd () in
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
  let t =
    { visible
    ; set_visible
    ; generation = Bonsai.Expert.Var.value generation_var
    ; generation_var
    ; client
    ; dimensions
    ; cwd
    ; on_show
    ; on_hide
    ; on_ctrl_c
    }
  in
  (* The shell exited: close the overlay (and refresh — the user probably
     just ran a mutating command). *)
  let closed =
    (* named and consumed: an ignored binding is PRUNED by ppx_bonsai and
       the check would never re-run (see memory: views stop updating). *)
    let%arr generation = t.generation in
    ignore (generation : int);
    match !client with
    | Some c -> Terminal.Session.is_closed c
    | None -> false
  in
  Bonsai.Edge.on_change
    ~equal:Bool.equal
    ~callback:
      (let%arr set_visible and visible and on_hide in
       fun closed ->
         if closed && visible then Effect.Many [ set_visible false; on_hide ] else Effect.Ignore)
    closed
    graph;
  t
;;

module Controls = struct
  type t = { toggle : unit Effect.t }
end

let controls t =
  (* No generation dep: all session reads happen at effect time. *)
  let%arr visible = t.visible
  and set_visible = t.set_visible
  and on_show = t.on_show
  and on_hide = t.on_hide
  and dimensions = t.dimensions in
  let bump () = Bonsai.Expert.Var.update t.generation_var ~f:(fun g -> g + 1) in
  { Controls.toggle =
      (if visible
       then Effect.Many [ set_visible false; on_hide ]
       else
         Effect.of_thunk (fun () ->
           (match !(t.client) with
            | Some c when not (Terminal.Session.is_closed c) -> ()
            | Some _ | None ->
              let rows, cols = inner dimensions in
              t.client
              := Some
                   (Terminal.Session.create
                      ~command:(shell ())
                      ~cwd:t.cwd
                      ~rows
                      ~cols
                      ~publish:(fun () ->
                        bump ();
                        Wake.wake ())
                      ~on_closed:(fun () ->
                        bump ();
                        Wake.wake ())));
           bump ())
         (* [on_show] first: taking the screen is a focus-stealing action
            — an active search prompt implicitly commits (L3). *)
         |> fun spawn -> Effect.Many [ on_show; spawn; set_visible true ])
  }
;;

let wrap t (base : Widget.screen) : Widget.screen =
  let toggle = controls t in
  let view =
    let%arr base_view = base.Widget.view
    and visible = t.visible
    and generation = t.generation in
    ignore (generation : int);
    if not visible
    then base_view
    else (
      let inner_view =
        match !(t.client) with
        | Some c -> Terminal.Session.render c
        | None -> View.text ~attrs:Theme.context " starting shell..."
      in
      let boxed =
        View.zcat
          [ View.pad ~l:2 (View.text ~attrs:Theme.header " terminal — Ctrl-T: hide ")
          ; Bonsai_term_border_box.view
              ~line_type:Round_corners
              ~attrs:Theme.border_focused
              inner_view
          ]
      in
      View.zcat [ View.pad ~l:margin_x ~t:margin_y boxed; base_view ])
  in
  let handler =
    let%arr base_handler = base.Widget.handler
    and visible = t.visible
    and { Controls.toggle } = toggle
    and on_ctrl_c = t.on_ctrl_c
    and dimensions = t.dimensions in
    fun (event : Event.t) ->
      let forward event =
        match !(t.client) with
        | Some c when not (Terminal.Session.is_closed c) ->
          Effect.of_thunk (fun () ->
            match event with
            | Event.Mouse { kind; position; mods } ->
              (* Pane-local coordinates inside padding + border. *)
              let x = position.x - margin_x - 1
              and y = position.y - margin_y - 1 in
              let rows, cols = inner dimensions in
              if x >= 0 && y >= 0 && x < cols && y < rows
              then
                Terminal.Session.send_event
                  c
                  (Event.Mouse { kind; position = { Position.x; y }; mods })
            | event -> Terminal.Session.send_event c event)
        | Some _ | None -> Effect.Ignore
      in
      match visible, event with
      | _, event when Chord.ctrl 't' event -> toggle
      | false, event -> base_handler event
      (* Ctrl-C while visible: SIGINT for the shell, never quit — plus a
         hint at the way out. *)
      | true, event when Chord.ctrl 'c' event -> Effect.Many [ forward event; on_ctrl_c ]
      | true, event -> forward event
  in
  { base with Widget.view; handler }
;;
