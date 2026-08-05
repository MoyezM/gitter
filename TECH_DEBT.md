# Tech debt

Known issues we've found and deliberately deferred. Add entries as they're
discovered: what breaks, where, and the fix sketch — so picking one up
later doesn't require re-diagnosis.

## FIXED 2026-08-05 (the sweep)

For the record, resolved in the tech-debt sweep:

- **Snapshot-operate races**: every effectful key (files s/u/d/y/r, diff
  hunk s/u and y, stack Enter-set-base) now routes through its state
  machine as an `Operate`/`Enter` action; the HOST's apply_action wrapper
  resolves the target against the CURRENT model and fires via
  `Apply_action_context.schedule_event`. Ops ride the machine INPUT
  (per-section records built inside Git_data; the app injects
  `discard_confirm`/`copy_path`/`set_notice` since modal/tty/notice are
  the app's). Verified live: `[j, s]` in one input batch stages the row
  j moved TO.
- **Copy feedback**: the status-bar notice now carries a kind — red
  errors, dim `Info` flashes; `y` flashes "copied <path>" which
  self-clears after 2.5s unless something newer replaced it.
- **Divider-fraction drift**: solver split keys are content-derived
  (leftmost leaf of each side, e.g. "staged|changes") instead of
  positional, so dragged fractions survive pane hiding.
- **Build noise**: `-Wl,-w` link flag on bin/test/bench (the opam-switch
  objects target a newer macOS SDK than the nix clang links against —
  cosmetic); vendored grammar C compiles silence upstream
  unused-but-set-variable warnings (scripts/add-grammar emits the flag
  for future grammars too).

## Still open

- Cursor-moving actions bake the snapshot pane HEIGHT into their payload
  (`Move/Activate/... { height; _ }`) — a resize sharing the input batch
  reveals against the old height; self-corrects on the next action.
  Cosmetic cousin of the fixed race class.
- Staged-side `y` in the diff pane copies the INDEX-side line number; if
  the worktree has further unstaged edits above that line, the jump
  target is off by those edits.
- The OSC 52 clipboard fallback (no tool on PATH) is untested against
  real terminals; Terminal.app ignores OSC 52 silently.
- Stack pane: slash-prefix grouping is one level deep; fold overrides
  for deleted branches/groups are never pruned from the model (harmless,
  unbounded only in theory).
- Stack inference needs REFLOGS to re-associate a child with an amended
  parent — fresh clones (no reflog history) degrade to trunk-child until
  the next restack.
