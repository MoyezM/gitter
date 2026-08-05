open! Core

(* VSCode's split: the index side stages a file; the worktree side (or being
   untracked/conflicted) keeps it in Changes. A file can be in both. Each
   section is its own LAYOUT PANE — the pane system provides the
   independent scrolling, focus, and resize. *)

let is_staged (e : Git.Status.Entry.t) =
  match e.kind with
  | Untracked | Unmerged -> false
  | Changed | Renamed _ -> not (Char.equal e.index '.')
;;

let is_unstaged (e : Git.Status.Entry.t) =
  match e.kind with
  | Untracked | Unmerged -> true
  | Changed | Renamed _ -> not (Char.equal e.worktree '.')
;;
