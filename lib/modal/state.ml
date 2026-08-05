open! Core
open! Bonsai_term

(* The single app-level modal. The stored effects are the continuation —
   the opener decides what confirming or submitting means, the modal only
   owns the interaction. *)

module Model = struct
  type t =
    | Closed
    | Confirm of
        { title : string
        ; body : string
        ; on_confirm : unit Effect.t
        }
    | Input of
        { title : string
        ; text : string
        ; on_submit : string -> unit Effect.t
        }
end

(* Transitions go through a state machine — NOT a per-frame closure over
   the model: rapid typing delivers many keys per frame, and folding them
   through a snapshot collapses the burst to its last character. Submission
   reads the CURRENT model via peek in the component. *)
module Action = struct
  type t =
    | Open of Model.t
    | Close
    | Append of char
    | Backspace

  let sexp_of_t = function
    | Open _ -> Sexp.Atom "Open"
    | Close -> Sexp.Atom "Close"
    | Append c -> Sexp.List [ Atom "Append"; Atom (String.of_char c) ]
    | Backspace -> Sexp.Atom "Backspace"
  ;;
end

let apply_action (model : Model.t) (action : Action.t) =
  match action, model with
  | Action.Open m, _ -> m
  | Close, _ -> Model.Closed
  | Append c, Input { title; text; on_submit } ->
    Model.Input { title; text = text ^ String.of_char c; on_submit }
  | Backspace, Input { title; text; on_submit } ->
    Model.Input { title; text = String.drop_suffix text 1; on_submit }
  | (Append _ | Backspace), (Closed | Confirm _) -> model
;;
