open! Core
open Async

(* Review marks on disk: SQLite at <git common dir>/gitter/gitter.db — the
   COMMON dir so worktrees share one database (the spec's decision;
   invisible to the worktree). The schema is the minimal reviews table,
   keyed on the content pair; it grows per milestone (PR comments,
   sessions) — which is why this is a database and not a flat file.

   Concurrency: every operation opens its own handle, does its work in
   [In_thread.run] (the sqlite3 5.4 stubs release the OCaml runtime lock
   around C calls), and closes — marks change at human speed, and
   open-per-op sidesteps all cross-thread handle rules. *)

module S = Sqlite3

let db_path () =
  match%map
    Process.run ~prog:"git" ~args:[ "rev-parse"; "--git-common-dir" ] ()
  with
  | Error e -> Error (Error.tag e ~tag:"review store: not a git repository")
  | Ok out ->
    let dir = Filename.concat (String.strip out) "gitter" in
    Ok (Filename.concat dir "gitter.db")

let schema =
  "CREATE TABLE IF NOT EXISTS reviews (\n\
  \  old_blob TEXT NOT NULL,\n\
  \  new_blob TEXT NOT NULL,\n\
  \  marked_at TEXT,\n\
  \  PRIMARY KEY (old_blob, new_blob))"
;;

(* Open, ensure schema, run, close — never leaks the handle. [Rc.check]
   accepts only OK/DONE, so SELECT loops match [step] by hand. *)
let with_db path f =
  In_thread.run (fun () ->
    Or_error.try_with (fun () ->
      Core_unix.mkdir_p (Filename.dirname path);
      let db = S.db_open path in
      Exn.protect
        ~f:(fun () ->
          S.Rc.check (S.exec db schema);
          f db)
        ~finally:(fun () -> ignore (S.db_close db : bool))))
;;

let load () =
  match%bind db_path () with
  | Error _ as e -> return e
  | Ok path ->
    with_db path (fun db ->
      let stmt = S.prepare db "SELECT old_blob, new_blob FROM reviews" in
      Exn.protect
        ~f:(fun () ->
          let rec rows acc =
            match S.step stmt with
            | S.Rc.ROW -> rows ((S.column_text stmt 0, S.column_text stmt 1) :: acc)
            | DONE -> List.rev acc
            | rc -> failwith ("review store: " ^ S.Rc.to_string rc)
          in
          rows [])
        ~finally:(fun () -> S.Rc.check (S.finalize stmt)))
;;

let set ~pairs ~reviewed =
  match%bind db_path () with
  | Error _ as e -> return e
  | Ok path ->
    with_db path (fun db ->
      let sql =
        if reviewed
        then "INSERT OR REPLACE INTO reviews VALUES (?, ?, ?)"
        else "DELETE FROM reviews WHERE old_blob = ? AND new_blob = ?"
      in
      let now = Time_float.to_string_utc (Time_float.now ()) in
      List.iter pairs ~f:(fun (old_blob, new_blob) ->
        let stmt = S.prepare db sql in
        Exn.protect
          ~f:(fun () ->
            S.Rc.check (S.bind_text stmt 1 old_blob);
            S.Rc.check (S.bind_text stmt 2 new_blob);
            if reviewed then S.Rc.check (S.bind_text stmt 3 now);
            match S.step stmt with
            | S.Rc.DONE -> ()
            | rc -> failwith ("review store: " ^ S.Rc.to_string rc))
          ~finally:(fun () -> S.Rc.check (S.finalize stmt))))
;;
