open! Core

type row =
  | Dir of
      { path : string
      ; name : string
      ; depth : int
      ; expanded : bool
      }
  | File of
      { entry : Git.Status.Entry.t
      ; name : string
      ; depth : int
      }

let depth = function
  | Dir { depth; _ } -> depth
  | File { depth; _ } -> depth
;;

(* Group (entry, remaining-path) pairs by their first path segment. Sorting
   by path first is what makes a directory's entries consecutive — porcelain
   emits tracked and untracked entries as two separately-sorted sections, so
   the input interleaves directories across sections. A group whose
   remainders contain no '/' is a run of plain files; otherwise it is one
   directory, recursed into when expanded. *)
let rows ~(entries : Git.Status.Entry.t list) ~collapsed =
  let entries =
    List.sort entries ~compare:(fun (a : Git.Status.Entry.t) b ->
      String.compare a.path b.path)
  in
  let seg rest =
    match String.lsplit2 rest ~on:'/' with
    | Some (s, _) -> s
    | None -> rest
  in
  let rec build pairs ~dir ~depth =
    List.group pairs ~break:(fun (_, a) (_, b) -> not (String.equal (seg a) (seg b)))
    |> List.concat_map ~f:(fun group ->
      let _, rest0 = List.hd_exn group in
      if not (String.contains rest0 '/')
      then List.map group ~f:(fun (entry, name) -> File { entry; name; depth })
      else (
        let name = seg rest0 in
        let path = if String.is_empty dir then name else dir ^ "/" ^ name in
        let expanded = not (Set.mem collapsed path) in
        let children =
          if not expanded
          then []
          else (
            let strip (e, rest) = e, String.drop_prefix rest (String.length name + 1) in
            build (List.map group ~f:strip) ~dir:path ~depth:(depth + 1))
        in
        Dir { path; name; depth; expanded } :: children))
  in
  build (List.map entries ~f:(fun e -> e, e.Git.Status.Entry.path)) ~dir:"" ~depth:0
;;
