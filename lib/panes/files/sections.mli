(** The staged/unstaged classification, VSCode-style: the index side stages
    a file; the worktree side (or being untracked/conflicted) keeps it in
    Changes. A file with both kinds of changes appears in both panes. *)

val is_staged : Git.Status.Entry.t -> bool
val is_unstaged : Git.Status.Entry.t -> bool
