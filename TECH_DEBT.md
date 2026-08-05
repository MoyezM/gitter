# Tech debt

Known issues we've found and deliberately deferred. Add entries as they're
discovered: what breaks, where, and the fix sketch — so picking one up
later doesn't require re-diagnosis.

## Same-frame event races (snapshot-operate class) — 2026-08-05

The recurring bonsai burst-collapse class, found by adversarial review but
deferred: handlers that resolve a TARGET from the `%arr`-captured model
snapshot race against same-frame state-machine transitions. The driver
folds all queued events through one handler closure per frame with no
flush in between, so two keys inside one ~8ms frame (held `j` repeat plus
a quick press) compute from the same stale model.

- `lib/panes/files/component.ml` `operate` (s/u/d/y): a `[j, s]` batch
  stages the row the cursor just LEFT; `[j, d]` opens the discard confirm
  for the wrong path (the modal body showing the path is the only
  mitigation); `[h, s]` stages the child file after the cursor jumped to
  its parent dir.
- `lib/panes/diff/component.ml` `hunk_op` (s/u) and the `y` arm: a
  `[j, s]` batch stages the previous hunk; `[j, y]` copies the previous
  row's line number.
- Minor cousin: cursor-moving actions bake the snapshot pane height into
  their payload (`Move/Activate/Collapse/Wheel { height; _ }`), so a
  resize sharing the batch reveals against the old height (self-corrects
  on the next action).

**Fix sketch (do NOT use `Bonsai.peek` — queued actions apply at the next
flush, so peek still reads the stale model):** route the operations
through the state machines. Add an `Operate of [ `Stage | `Unstage |
`Discard | `Copy ]`-style action, carry the op effects in the
state-machine INPUT, and fire them from `apply_action` via
`Bonsai.Apply_action_context.schedule_event` so the target resolves
against the current model. Touches both panes plus `Git_data` and the
app wiring.

## Copy-path rough edges — 2026-08-05

- No feedback on `y`: the copy is silent. The status-bar `notice` channel
  is error-styled (red) with no expiry and is owned by `Git_data.mutate`;
  a "copied <path>" confirmation wants a neutral style and a timeout, or
  a separate app-level notice.
- Staged-side `y` in the diff pane copies the INDEX-side line number; if
  the worktree has further unstaged edits above that line, the jump
  target is off by those edits.
- The OSC 52 fallback (no clipboard tool on PATH) is untested against
  real terminals; Terminal.app ignores OSC 52 silently, so on a bare
  Linux box over Terminal.app-ssh the copy would no-op with no error.

## Stack pane rough edges — 2026-08-05

- Divider fractions are keyed by POSITIONAL path ("0", "0/1"): hiding the
  stack pane collapses the left column's split tree, so a fraction the
  user dragged for one boundary gets reapplied to a different one after
  toggling. Cosmetic (re-draggable). Clean fix: derive split keys from
  the first leaf id of the split's `first` subtree in Solver so keys
  survive pruning.
- The stack inference needs REFLOGS to associate a child with an amended
  parent (the old tip only survives there). Fresh clones and repos with
  `core.logAllRefUpdates=false` lose that association — the child shows
  as a trunk child with no needs-restack flag until the next restack.
- Slash-prefix grouping is one level deep (first segment only): nested
  prefixes like team/feature/x group under "team/" without a second
  level. Fine until someone's naming scheme is deeper.
- Fold overrides are keyed by branch/group name and never pruned:
  deleting a branch leaves its stale override entry in the model
  (harmless, invisible, unbounded only in theory).

## Build noise — 2026-08-05

- Every link floods `ld: warning: object file ... built for newer macOS
  version (26.0) than being linked (14.0)`: the opam-switch objects
  target the host SDK while the nix clang wrapper pins the nixpkgs
  apple-sdk (14.4). Cosmetic. Fix: pin a newer `apple-sdk` in the flake
  devShell or silence with `-Wl,-w` in an env stanza.
- Vendored grammar C compiles warn (`-Wunused-but-set-variable` in
  upstream parser/scanner sources). Cosmetic; suppressing per-grammar
  would belong in `scripts/add-grammar`'s generated dune.
