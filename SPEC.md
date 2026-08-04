# gitter — product spec

> A git TUI tailored to my workflow. Working document.

## Problem / motivation

Pain points with lazygit today:

1. **Reviewing committed code is hard.** Once code is committed (especially
   across a stack), there's no good way to view/review it.
2. **No editor round-trip.** There's no great way to quickly pop open the
   editor to dig into code (grep references, jump around) and then pop back
   to where you were.
3. **No GitHub integration.** On some timeline: adding GitHub PR comments and
   proper GitHub integration from the TUI would be epic.

## My workflow

- Stacked PRs via Graphite (`gt`). Each branch is based on a parent branch;
  the diff *relative to the base* is what matters, not just working-tree
  changes.
- Increasingly reviewing code written by agents — need to review incrementally
  and keep track of what's been looked at.

## Goals

- Make reviewing committed/stacked code a first-class flow (not an
  afterthought like in lazygit).
- Fast stage/unstage loop for shaping commits.
- Seamless escape hatch to $EDITOR and back.

## Non-goals

<!-- TBD. Candidates: full lazygit replacement (rebase UI, reflog, etc.)?
     Non-Graphite branching models? -->

## Core features

### Left pane: file tree

Two modes:

1. **Default mode** — staged + unstaged files.
   - Toggle between flat list and tree view.
   - See at a glance what is staged vs unstaged.
   - Stage/unstage whole files and individual hunks.
2. **Relative mode** — diff relative to a chosen base branch.
   - Generic form: pick any branch to compare against; the Graphite parent
     is the **shortcut/default** (read from `gt` metadata).
   - Treated as a **review mode**: mark files as reviewed.
   - Acts like a stack viewer for incrementally reviewing code (typically
     from agents). Visualizing the current stack is a nice touch.

### Right pane: diff view

- Nice git diff for the selected file.
- Syntax highlighting (tree-sitter or similar).

### Editor integration

- Open the selected file in `$EDITOR`, do heavy digging (grep references,
  etc.), then pop back into gitter where you left off.

### GitHub integration (later)

- Add PR comments from the TUI; proper GitHub integration.

## Decisions

- **Relative mode base** (decided): choose any branch to compare against;
  parent-of-current-branch (via Graphite metadata) is the shortcut/default.
  Stack visualization is a nice-to-have on top.
- **Reviewed marks** (decided):
  - **Key marks by content, not by commit**: a mark is keyed on the
    *(base blob hash, current blob hash)* pair for the file — i.e. exactly
    the diff that was reviewed. Git content-addresses blobs, so marks
    survive `gt restack`/amends that don't change the file, and
    auto-invalidate the moment either side's content changes. No explicit
    invalidation logic needed, regardless of storage backend.
  - **Storage: embedded SQLite** (e.g. `.git/gitter/gitter.db` in the git
    common dir — invisible to the worktree, correct across worktrees).
    Chosen over a flat file because the data model is expected to grow:
    replicating GitHub PR comments locally, review sessions, possibly
    hunk-level marks — a local replica/cache is what SQLite is for.
    Start with a minimal schema (reviews table only); grow per milestone.
  - Git notes rejected: notes attach to commit objects, and Graphite
    rewrites the whole stack constantly — marks would orphan on every
    restack. Notes also don't sync by default and the tooling is clunky.

## Open questions

- **Editor round-trip mechanics**: suspend the TUI and exec `$EDITOR`
  (lazygit-style), or use the embedded-terminal component we already have
  working so gitter stays visible? What does "pop back" restore (selection,
  scroll position, mode)?
- **Syntax highlighting implementation**: tree-sitter bindings under the
  OxCaml switch vs shelling out to an existing highlighter (`delta`,
  `difftastic`, `bat`) for v0.
- **Hunk staging UX**: inline in the diff pane (stage hunk under cursor)?
- **Marks across machines**: local-only is fine for v2; if marks ever need
  to travel, add explicit export/import or a sync ref later.

## UX sketches

Very rough starting point:

```
┌ files ──────────────┬ diff: src/foo.ml ───────────────────────────┐
│ [default|relative]  │ @@ -12,6 +12,9 @@                           │
│                     │   let start () =                            │
│ ▾ src/              │ +   let config = load_config () in          │
│   M foo.ml          │ +   run ~config                             │
│   M bar.ml (staged) │ -   run ()                                  │
│ ▾ test/             │                                             │
│   A foo_test.ml     │        (syntax highlighted)                 │
│                     │                                             │
│ [relative mode:     │                                             │
│  ✓ reviewed marks]  │                                             │
└─────────────────────┴─────────────────────────────────────────────┘
```

## Architecture notes

- bonsai_term app; left/right panes as components.
- How we talk to git: shell out to `git` (and `gt`?) vs a libgit binding —
  TBD; shelling out is the pragmatic default.
- The embedded-terminal component (`bonsai_term_components.tmux`) is already
  wired and may serve the editor round-trip.
- SQLite: needs an OCaml binding (`sqlite3` is the standard; `caqti` if we
  want typed queries) built under the OxCaml switch, plus libsqlite3 +
  headers exposed via the nix config (same pattern as gmp).

## Milestones

Draft, for discussion:

- **M0 (interactive mock)**: full two-pane UI over hardcoded fake data —
  mode toggle, flat/tree toggle, selection, in-memory stage/unstage and
  reviewed marks, canned diffs with hand-styled colors, status bar.
  No git, no SQLite, no tree-sitter. Success = driving the imagined
  workflow with keys feels right; components carry forward to v0 as-is.
- **v0 (read-only)**: swap fake data for real git — two-mode file tree +
  diff pane with highlighting. No mutations — just *seeing* clearly.
- **v1 (staging)**: stage/unstage files and hunks in default mode.
- **v2 (review)**: reviewed marks with persistence + stack awareness.
- **v3 (editor)**: $EDITOR round-trip.
- **v4 (github)**: PR comments / GitHub integration.
