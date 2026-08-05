# The embedded terminal component

LIVE (revived 2026-08-05): `Vterm` wraps **libvterm** (the emulator
neovim embeds; `vterm_stubs.c`), `Session` runs a command on a forkpty'd
pty (`pty_stubs.c`). The consumer is `../term_pane/` — the SHELL OVERLAY
(Ctrl-T): a padded modal terminal running `$SHELL` in the repo root for
quick git/gt commands; hiding or shell exit fires an immediate git
refresh. (Its earlier life as an `$EDITOR` host is in git history.)

It worked well when shelved — verified end to end:
keystroke→painted echo 0.7ms median / 3.4ms max, zero >33ms gaps in a
2.8MB output flood, 0.0% idle CPU, real mouse pass-through (SGR, encoded
by libvterm per the app's negotiated modes).

Known constraint baked into `session.ml`: macOS kqueue cannot watch pty
master fds, so IO is blocking `read(2)` on a pool thread and synchronous
`write(2)` — do not "simplify" it back to Async `Reader`/`Writer`.

Colors: indexed colors (ANSI 16 + xterm-256) pass through SYMBOLICALLY —
never `vterm_screen_convert_color_to_rgb` — so the HOST terminal resolves
them against the user's palette (the tmux trick; converting paints
libvterm's alien stock palette). Default fg/bg emit no attribute at all,
inheriting the host default including translucency. Only true-color SGR
from the app flows as RGB.
