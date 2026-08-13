open! Core

module Recall = struct
  type t =
    { prefix : string
    ; index : int
    }
  [@@deriving sexp_of, equal]
end

type t =
  { typed : string option
  ; register : string option
  ; recall : Recall.t option
  }
[@@deriving sexp_of, equal]

let idle = { typed = None; register = None; recall = None }
let is_active t = Option.is_some t.typed

let query t =
  match t.typed with
  | Some typed -> Some typed
  | None -> t.register
;;

let parsed t = Option.map (query t) ~f:Query.parse

let match_query t =
  match parsed t with
  | Some query when not (Query.is_empty query) -> Some query
  | Some _ | None -> None
;;

type event =
  | Open
  | Type of string
  | Backspace
  | Commit
  | Implicit_commit
  | Cancel
  | Clear
  | Recall_prev
  | Recall_next
[@@deriving sexp_of]

let utf8 = Uchar.Utf8.to_string

(* Backspace deletes one CODEPOINT: strip the trailing continuation
   bytes, then the lead byte. *)
let drop_last_scalar s =
  let n = String.length s in
  if n = 0
  then s
  else (
    let i = ref (n - 1) in
    while !i > 0 && Char.to_int s.[!i] land 0xC0 = 0x80 do
      decr i
    done;
    String.prefix s !i)
;;

(* Commits push the resulting register onto the global history; [push]
   dedups against the head, which is what keeps L2's ghost re-commit
   from duplicating. *)
let committed register =
  Option.iter register ~f:History.push;
  { typed = None; register; recall = None }
;;

let apply t event =
  match event, t.typed with
  | Open, None -> { t with typed = Some ""; recall = None }
  | Open, Some _ -> t
  | Type s, Some typed -> { t with typed = Some (typed ^ s); recall = None }
  | Backspace, Some typed ->
    (* No-op on an empty query — the ghost is display-only. *)
    { t with typed = Some (drop_last_scalar typed); recall = None }
  | Commit, Some typed ->
    (* Enter on the empty prompt re-commits the ghost; with no register
       it just closes. *)
    committed (if String.is_empty typed then t.register else Some typed)
  | Implicit_commit, Some typed ->
    (* The ghost re-commits only on an explicit Enter (L3). *)
    if String.is_empty typed
    then { t with typed = None; recall = None }
    else committed (Some typed)
  | Cancel, Some _ -> { t with typed = None; recall = None }
  | Clear, None -> { t with register = None }
  | Recall_prev, Some typed ->
    let prefix, index =
      match t.recall with
      | Some { Recall.prefix; index } -> prefix, index + 1
      | None -> typed, 0
    in
    (match History.recall ~prefix index with
     | Some entry -> { t with typed = Some entry; recall = Some { Recall.prefix; index } }
     | None -> t)
  | Recall_next, Some _ ->
    (match t.recall with
     | None -> t
     | Some { Recall.prefix; index } ->
       (match if index = 0 then None else History.recall ~prefix (index - 1) with
        | Some entry ->
          { t with typed = Some entry; recall = Some { Recall.prefix; index = index - 1 } }
        (* Stepping past the newest returns to the typed prefix. *)
        | None -> { t with typed = Some prefix; recall = None }))
  | (Type _ | Backspace | Cancel | Recall_prev | Recall_next | Commit | Implicit_commit), None
  | Clear, Some _ -> t
;;
