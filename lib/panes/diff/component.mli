open! Core
open! Bonsai_term

(** The diff pane: shows the selected file's staged (index vs HEAD) or
    unstaged (worktree vs index) change per the selection's side, syntax
    highlighted, with a helix-feel cursor (j/k/arrows, Ctrl-d/Ctrl-u and
    brackets for half-pages, click; the wheel scrolls the viewport without moving the
    cursor, which may end up off-screen). [s] stages / [u] unstages the
    hunk under the cursor (side-appropriate only); [y] runs [copy_path]
    with "path:LINE" for the cursor's line (the jump format editors
    accept); [revision] bumps trigger a refetch after index mutations.

    [/] searches the whole interleaved file — including context git
    never shipped, and removed lines. Jumps ([↑]/[↓] mid-prompt,
    [n]/[N] committed) auto-reveal a hidden match minimally and
    locally; the border shows total, current, and hidden counts. The
    layout reads the search surface off the returned [Widget.leaf]
    (L3). *)

val component
  :  selection:Fetch.key option Bonsai.t
  -> revision:int Bonsai.t
  -> stage_hunk:(path:string -> raw:string -> unit Effect.t) Bonsai.t
  -> unstage_hunk:(path:string -> raw:string -> unit Effect.t) Bonsai.t
  -> copy_path:(string -> unit Effect.t) Bonsai.t
  -> Widget.leaf
