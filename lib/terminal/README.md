# Archived: the embedded terminal component

A generic embedded-terminal component — `Vterm` wraps **libvterm** (the
emulator neovim embeds; `vterm_stubs.c`), `Session` runs a command on a
forkpty'd pty (`pty_stubs.c`). `../term_pane/` is the fullscreen overlay
consumer that hosted `$EDITOR` until the feature was removed in favor of
`y` (copy path). Both directories are excluded from the build via the
`(dirs :standard \ terminal term_pane)` stanza in `lib/dune` so gitter no
longer links libvterm.

It worked well when shelved — verified end to end:
keystroke→painted echo 0.7ms median / 3.4ms max, zero >33ms gaps in a
2.8MB output flood, 0.0% idle CPU, real mouse pass-through (SGR, encoded
by libvterm per the app's negotiated modes).

## To revive

1. Remove `terminal term_pane` from the `dirs` stanza in `lib/dune`.
2. Move `pty_stubs.c` and `vterm_stubs.c` back to `lib/` and restore in
   the `library` stanza:
   ```
   (foreign_stubs
    (language c)
    (names pty_stubs vterm_stubs)
    (flags (:standard (:include vterm_c_flags.sexp))))
   (c_library_flags (:include vterm_c_library_flags.sexp))
   (rule
    (targets vterm_c_flags.sexp vterm_c_library_flags.sexp)
    (action (run %{exe:../config/discover.exe})))
   ```
   and move `config/` back to the repo root (the rule path expects it).
3. Add `libvterm-neovim` back to the flake devShell.
4. Move `test_terminal.ml` / `test_session.ml` back to `test/` and add
   them (plus `async bonsai_term` libraries) to `test/dune`.
5. Wire the feature: `Term_pane.Component.create/controls/wrap` in
   `app.ml` (see git history), keybinding + status-bar hints.
6. Restore the frame-loop wakeup: move `wake.ml`/`wake.mli` (archived in
   this directory) back to `lib/`, switch `bin/main.ml` from plain
   `Bonsai_term.start` back to `start_with_driver` and install
   `Gitter.Wake.set (fun () -> Driver.send_incoming_event driver ())`
   (see git history). CAUTION: `start_with_driver` does not add Ctrl-C
   exit — reuse `Bonsai_term.Private.For_testing.make_app_exit_on_ctrlc`
   rather than matching the key yourself; notty reports Ctrl-C as
   UPPERCASE `ASCII 'C'`, and a hand-rolled lowercase match silently
   never fires (this happened).

Known constraint baked into `session.ml`: macOS kqueue cannot watch pty
master fds, so IO is blocking `read(2)` on a pool thread and synchronous
`write(2)` — do not "simplify" it back to Async `Reader`/`Writer`.
