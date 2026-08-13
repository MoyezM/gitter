open! Core
open! Bonsai_term

type payload =
  { source : Document.Source.t
  ; files : Git.Diff.File.t list
    (* the parsed diff — how Render.content tells an empty diff from a
       binary or mode-only one; staging reads Source.hunks instead *)
  ; old_hl : Highlight.t
  ; new_hl : Highlight.t
  ; doc_id : int
    (* stable across the two phases of one load, distinct per load — the
       value form of "same document, highlights swapped in" vs "document
       replaced"; the component re-anchors its cursor when it changes *)
  }

type side =
  [ `Staged
  | `Unstaged
  | `Committed of string
  ]
[@@deriving equal]

type key = string * side [@@deriving equal]
type result = (key * payload Or_error.t) option

type t = Latest.t

let create () = Latest.create ()

(* Phase one of a fetch: the diff and both blob contents — no
   highlighting. Which git object each side reads per section is
   [Git.Queries.diff_with_contents]'s law. *)
let fetch_text ((path, side) : key) = Git.Queries.diff_with_contents side ~path

(* The two-phase load protocol: one [Latest] token per load, every write
   (and the start of every phase) guarded by [when_current] — a
   superseded fetch must not write (a slow fetch for a de-selected, or
   quickly re-selected, file landing last would wedge or stale the pane)
   and stops early, skipping highlight parses for files no longer on
   screen. The token doubles as the payload's [doc_id]. *)
let load t ~key ~set =
  let path, _side = key in
  let%bind.Effect mine = Latest.start t in
  let when_current eff = Latest.when_current t mine eff in
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
       (* Pre-warming the empty-levels mask keeps the FIRST materialization
          in the domain too: a selection change resets levels before the
          load, so the pane's first ask is a memo hit, not an O(rows) walk
          on the scheduler right as the fetch lands. *)
       let build () =
         let source =
           Document.Source.create files ~old_text:old_content ~new_text:new_content
         in
         ignore (Document.of_source source ~levels:Int.Map.empty : Document.t);
         source
       in
       let%bind.Effect source =
         Effect.of_deferred_thunk (fun () -> Cpu.in_domain ~fallback:build build)
       in
       (* Phase one: diff text renders immediately, plain. *)
       let%bind.Effect () =
         when_current
           (set
              (Some
                 ( key
                 , Ok
                     { source
                     ; files
                     ; old_hl = Highlight.empty
                     ; new_hl = Highlight.empty
                     ; doc_id = mine
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
       when_current (set (Some (key, Ok { source; files; old_hl; new_hl; doc_id = mine }))))
;;

let clear t ~set =
  let%bind.Effect () = Latest.invalidate t in
  set None
;;
