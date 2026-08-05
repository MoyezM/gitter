open! Core
open! Bonsai_term

type payload =
  { document : Document.t
  ; files : Git.Diff.File.t list (* kept for hunk staging: raw hunk bytes *)
  ; binary_only : bool
  ; old_hl : Highlight.t
  ; new_hl : Highlight.t
  }

type side =
  [ `Staged
  | `Unstaged
  | `Committed of string
  ]
[@@deriving equal]

type key = string * side [@@deriving equal]
type result = (key * payload Or_error.t) option

type t = { generation : int ref }

let create () = { generation = ref 0 }

(* Phase one of a fetch: the diff and both blob contents — no highlighting.
   The sides differ per section, VSCode-style: the Changes pane diffs the
   worktree against the INDEX (relative to what's staged), the Staged pane
   diffs the index against HEAD. The three reads are independent; start
   them all before binding (benchmarked at ~20-40ms each — serializing
   them tripled the latency floor of every fetch). *)
let fetch_text ((path, side) : key) =
  let open Async in
  let or_empty d =
    d
    >>| function
    | Ok c -> c
    | Error (_ : Error.t) -> "" (* that side doesn't exist for this file *)
  in
  let worktree =
    Monitor.try_with (fun () -> Reader.file_contents path)
    >>| function
    | Ok c -> c
    | Error _ -> "" (* deleted file *)
  in
  let diff, old_content, new_content =
    match side with
    | `Unstaged ->
      Git.Queries.diff_unstaged path, or_empty (Git.Queries.file_in_index path), worktree
    | `Staged ->
      ( Git.Queries.diff_staged path
      , or_empty (Git.Queries.file_at_head path)
      , or_empty (Git.Queries.file_in_index path) )
    | `Committed base ->
      ( Git.Queries.diff_committed ~base path
      , or_empty (Git.Queries.file_at_base ~base path)
      , or_empty (Git.Queries.file_at_head path) )
  in
  let%bind diff in
  let%bind old_content in
  let%bind new_content in
  return (Or_error.map diff ~f:(fun files -> files, old_content, new_content))
;;

(* The two-phase load protocol. Every write (and the start of every phase)
   is guarded by a per-fetch generation read at WRITE time: a superseded
   fetch must not write — a slow fetch for a de-selected (or quickly
   re-selected) file landing last would wedge or stale the pane — and stops
   early, skipping highlight parses for files no longer on screen. *)
let load t ~key ~set =
  let path, _side = key in
  let%bind.Effect mine =
    Effect.of_thunk (fun () ->
      incr t.generation;
      !(t.generation))
  in
  let when_current eff =
    let%bind.Effect current = Effect.of_thunk (fun () -> !(t.generation)) in
    if current = mine then eff else Effect.Ignore
  in
  let%bind.Effect fetched = Effect.of_deferred_thunk (fun () -> fetch_text key) in
  match fetched with
  | Error e -> when_current (set (Some (key, Error e)))
  | Ok (files, old_content, new_content) ->
    when_current
      ((* The document build runs off the scheduler (flattening a 646K-line
          diff costs ~1s and would stall the UI right as the fetch lands);
          the entry check above skips it entirely for already-superseded
          fetches. Domain exhaustion falls back to building on the
          scheduler — slow but correct; the pane must never wedge. *)
       let%bind.Effect document =
         Effect.of_deferred_thunk (fun () ->
           try Cpu.in_domain (fun () -> Document.of_files files) with
           | _ -> Async.return (Document.of_files files))
       in
       let binary_only = (not (List.is_empty files)) && Array.is_empty document in
       (* Phase one: diff text renders immediately, plain. *)
       let%bind.Effect () =
         when_current
           (set
              (Some
                 ( key
                 , Ok
                     { document
                     ; files
                     ; binary_only
                     ; old_hl = Highlight.empty
                     ; new_hl = Highlight.empty
                     } )))
       in
       (* Phase two: highlight sessions parse in their own domains and swap
          in when ready — frames never wait on a parse. *)
       let%bind.Effect old_hl, new_hl =
         Effect.of_deferred_thunk (fun () ->
           Async.Deferred.both
             (Highlight.create ~path old_content)
             (Highlight.create ~path new_content))
       in
       when_current (set (Some (key, Ok { document; files; binary_only; old_hl; new_hl }))))
;;

let clear t ~set =
  let%bind.Effect () = Effect.of_thunk (fun () -> incr t.generation) in
  set None
;;
