open! Core
open! Bonsai_term
open Bonsai.Let_syntax

(* App-root git state, per the hoisting pattern: the root owns cross-leaf
   data (status entries, the file cursor, the derived selection) and hands
   leaves the values plus injects. Leaves keep only presentation state.

   Loading: fetch fires whenever the state is [Not_loaded] (guarded by a
   [Loading] marker so it fires once), so [refresh] is just "reset to
   Not_loaded". *)

module Load = struct
  type t =
    | Not_loaded
    | Loading
    | Loaded of Git.Status.Entry.t list Or_error.t
end

let entries_of_load (load : Load.t) =
  match load with
  | Loaded (Ok entries) -> entries
  | Not_loaded | Loading | Loaded (Error _) -> []
;;

module Files_action = struct
  type t =
    | Move of [ `Up | `Down ]
    | Set of int
  [@@deriving sexp_of]
end

type t =
  { load : Load.t Bonsai.t
  ; refresh : unit Effect.t Bonsai.t
  ; cursor : int Bonsai.t
  ; files_inject : (Files_action.t -> unit Effect.t) Bonsai.t
  ; selection : string option Bonsai.t
  }

let create (local_ graph) =
  let load, set_load = Bonsai.state Load.Not_loaded graph in
  let fetch =
    let%arr load and set_load in
    match load with
    | Load.Not_loaded ->
      let%bind.Effect () = set_load Loading in
      let%bind.Effect result =
        Effect.of_deferred_thunk (fun () -> Git.Queries.status ())
      in
      set_load (Loaded result)
    | Loading | Loaded _ -> Effect.Ignore
  in
  Bonsai.Edge.before_display fetch graph;
  let refresh =
    let%arr set_load in
    set_load Load.Not_loaded
  in
  let cursor, files_inject =
    Bonsai.state_machine_with_input
      ~default_model:0
      ~apply_action:(fun _ctx input cursor (action : Files_action.t) ->
        let count =
          match input with
          | Bonsai.Computation_status.Active load -> List.length (entries_of_load load)
          | Inactive -> 0
        in
        let clamp c = Int.max 0 (Int.min (Int.max 0 (count - 1)) c) in
        match action with
        | Move `Up -> clamp (cursor - 1)
        | Move `Down -> clamp (cursor + 1)
        | Set i -> clamp i)
      load
      graph
  in
  let selection =
    let%arr load and cursor in
    List.nth (entries_of_load load) cursor
    |> Option.map ~f:(fun e -> e.Git.Status.Entry.path)
  in
  { load; refresh; cursor; files_inject; selection }
;;
