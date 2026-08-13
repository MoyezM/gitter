open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* Presentation plus event forwarding for ONE section's tree; the state
   lives in [Git_data] (the root owns it because the derived selection
   feeds the diff pane). Two instances — Staged and Changes — sit in
   their own layout panes, which is what makes them independently
   scrollable, focusable, and resizable. Search integration is
   [Search.Action]: the keymap, mode dispatch and lifecycle are the
   search's; only this pane's own bindings appear here. *)

module Input = struct
  type t =
    { status : Render.status Bonsai.t
    ; rows : Tree.row list Bonsai.t
    ; cursor : int Bonsai.t
    ; scroll : int Bonsai.t
    ; counts : (int * int) String.Map.t Bonsai.t
    ; reviewed : String.Set.t Bonsai.t
    ; side : [ `Staged | `Unstaged | `Committed ]
    ; search : Search.Prompt.t Bonsai.t
    ; search_counts : (int * int) Bonsai.t
    ; inject : (State.Action.t Search.Action.t -> unit Effect.t) Bonsai.t
    ; hints : string
      (* status-bar key hints — assembled by the host next to the ops
         wiring, the one place that knows which keys this section acts on *)
    }
end

let component
      { Input.status
      ; rows
      ; cursor
      ; scroll
      ; counts
      ; reviewed
      ; side
      ; search
      ; search_counts
      ; inject
      ; hints
      }
  : Widget.leaf
  =
  fun ~dimensions (local_ _graph) ->
  let view =
    let%arr status and rows and cursor and scroll and counts and reviewed and dimensions
    and search in
    Render.render ~status ~rows ~cursor ~scroll ~counts ~reviewed ~side ~search ~dimensions
  in
  (* The search prompt/register in the pane's bottom border (R1);
     [matches/total] counts on the right (T5) — the counts arrive as
     data because this pane's state is hosted. *)
  let border =
    let%arr search and search_counts and dimensions in
    Search.Tree_search.border_of_counts
      ~search
      ~counts:search_counts
      ~width:dimensions.width
  in
  let handler =
    let%arr inject and scroll and rows and dimensions in
    fun (event : Event.t) ->
      let height = dimensions.height in
      let pane = Search.Action.pane in
      let lift a = State.Action.Nav a in
      (* The idle reading of each printable: this pane's effectful keys
         (targets resolve at APPLY time — burst-safe), then the shared
         nav chars. *)
      let idle_char c =
        match c with
        | 's' -> Some (pane (State.Action.Operate Stage))
        | 'u' -> Some (pane (State.Action.Operate Unstage))
        | 'd' -> Some (pane (State.Action.Operate Discard))
        | 'c' -> Some (pane State.Action.Commit_prompt)
        | 'y' -> Some (pane (State.Action.Operate Copy_path))
        | 'r' -> Some (pane (State.Action.Operate Toggle_review))
        | c -> Tree_listing.idle_char ~height ~lift c
      in
      match Search.Action.keymap ~height ~idle_char event with
      | Some action -> inject action
      | None ->
        Tree_listing.handle ~height ~total:(List.length rows) ~scroll ~inject ~lift event
  in
  let search_active, commit_search = Search.Action.surface ~inject ~search in
  { Widget.view; border; search_active; commit_search; handler; hints }
;;
