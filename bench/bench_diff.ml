open! Core
open Async

(* Stage-by-stage timing of the diff-pane fetch pipeline (see
   lib/panes/diff/). Run: dune exec bench/bench_diff.exe -- <files...> *)

let ms span = Time_ns.Span.to_ms span

let time_sync name f =
  let start = Time_ns.now () in
  let r = f () in
  printf "  %-34s %8.1f ms\n" name (ms (Time_ns.diff (Time_ns.now ()) start));
  r
;;

let time_async name f =
  let start = Time_ns.now () in
  let%map r = f () in
  printf "  %-34s %8.1f ms\n" name (ms (Time_ns.diff (Time_ns.now ()) start));
  r
;;

let bench path =
  printf "=== %s ===\n" path;
  let%bind diff_out =
    time_async "git diff HEAD (subprocess)" (fun () ->
      Gitter.Git.Runner.git [ "diff"; "--no-color"; "HEAD"; "--"; path ]
      >>| Or_error.ok_exn)
  in
  printf "  %-34s %8d bytes\n" "(diff size)" (String.length diff_out);
  let files = time_sync "Diff.parse" (fun () -> Gitter.Git.Diff.parse diff_out) in
  let line_count =
    List.sum (module Int) files ~f:(fun f ->
      List.sum (module Int) f.hunks ~f:(fun h -> List.length h.lines))
  in
  printf "  %-34s %8d\n" "(diff lines)" line_count;
  let%bind (_ : string) =
    time_async "git show HEAD:file (subprocess)" (fun () ->
      Gitter.Git.Runner.git [ "show"; "HEAD:" ^ path ]
      >>| Result.ok >>| Option.value ~default:"")
  in
  let%bind new_content =
    time_async "read worktree file" (fun () ->
      Monitor.try_with (fun () -> Reader.file_contents path)
      >>| Result.ok >>| Option.value ~default:"")
  in
  printf "  %-34s %8d bytes\n" "(new content size)" (String.length new_content);
  (* Decompose parse vs query vs ranged-query using the C grammar directly:
     this is the decision data for viewport-based highlighting. *)
  (if String.is_suffix path ~suffix:".c"
   then (
     let language = Gitter_grammar_c.language () in
     let parser = time_sync "ts Parser.create" (fun () -> Tree_sitter.Parser.create language) in
     let tree =
       time_sync "ts parse ONLY" (fun () -> Tree_sitter.Parser.parse_string parser new_content)
     in
     let query =
       time_sync "ts Query.create" (fun () ->
         Tree_sitter.Query.create language ~source:Gitter_grammar_c.highlights_query)
     in
     let all = time_sync "ts query FULL file" (fun () -> Tree_sitter.highlight query tree) in
     printf "  %-34s %8d\n" "(captures, full)" (List.length all);
     let window =
       time_sync "ts query 4KB viewport" (fun () ->
         Tree_sitter.highlight_range query tree ~start_byte:100_000 ~end_byte:104_000)
     in
     printf "  %-34s %8d\n" "(captures, viewport)" (List.length window)));
  (* The production path: session create (parse, off-thread in the app) and
     a per-frame window query. *)
  let sess =
    time_sync "Highlight.create_sync (parse)" (fun () ->
      Gitter.Highlight.create_sync ~path new_content)
  in
  let (_ : Gitter.Highlight.Window.t) =
    time_sync "Highlight.window (50 lines)" (fun () ->
      Gitter.Highlight.window sess ~from_line:1000 ~to_line:1050)
  in
  (* Approximates the leaf's [flatten]: numbered lines for every hunk. *)
  let (_ : int) =
    time_sync "flatten (numbered display lines)" (fun () ->
      List.sum (module Int) files ~f:(fun f ->
        List.sum (module Int) f.hunks ~f:(fun h ->
          List.length (Gitter.Git.Diff.Hunk.numbered h))))
  in
  printf "\n";
  return ()
;;

let () =
  Command.async
    ~summary:"benchmark the diff fetch pipeline"
    (let%map_open.Command files = anon (sequence ("file" %: string)) in
     fun () ->
       let files =
         if List.is_empty files
         then
           [ "grammars/javascript/parser_javascript.c"
           ; "grammars/c/parser_c.c"
           ; "lib/app.ml"
           ]
         else files
       in
       Deferred.List.iter ~how:`Sequential files ~f:bench)
  |> Command_unix.run
;;
