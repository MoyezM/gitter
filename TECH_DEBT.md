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

## Build noise — 2026-08-05

- Every link floods `ld: warning: object file ... built for newer macOS
  version (26.0) than being linked (14.0)`: the opam-switch objects
  target the host SDK while the nix clang wrapper pins the nixpkgs
  apple-sdk (14.4). Cosmetic. Fix: pin a newer `apple-sdk` in the flake
  devShell or silence with `-Wl,-w` in an env stanza.
- Vendored grammar C compiles warn (`-Wunused-but-set-variable` in
  upstream parser/scanner sources). Cosmetic; suppressing per-grammar
  would belong in `scripts/add-grammar`'s generated dune.
