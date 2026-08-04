# gitter

A terminal UI built with [bonsai_term](https://github.com/janestreet/bonsai_term)
(Jane Street's Bonsai for terminals).

## Toolchain

bonsai_term requires the [OxCaml](https://oxcaml.org) compiler (it uses labeled
tuples and stack-allocation modes). It is installed in the `5.2.0+ox` opam
switch, created with:

```sh
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
opam install bonsai_term
```

System prerequisites (`autoconf`, `pkg-config`, `gmp`) are managed in the
devmachine-dotfiles nix config.

The `.envrc` activates the switch via direnv (`direnv allow` once), after which
plain `dune build` / `dune exec` work. Without direnv, prefix commands with
`opam exec --switch=5.2.0+ox --`.

## Run

```sh
dune exec bin/main.exe
```

The app has two focusable panes (Ctrl-T switches focus, Ctrl-C quits):

- **counter** — Up/Down arrows change it
- **terminal** — an embedded zsh, powered by `Bonsai_term_components.tmux`
  ("terminal iframe": it runs the command in a real tmux session, captures the
  pane every frame, and forwards your keys/mouse). Requires a `tmux` binary on
  PATH (nix-managed). Note: Ctrl-T and Ctrl-C are consumed by the host app and
  never reach the embedded program.

## Where to learn more

- The app shape: `bin/main.ml` — an app is a function
  `~dimensions -> graph -> ~view, ~handler`, started with `Bonsai_term.start`.
  There is one global handler (`Event.t -> unit Effect.t`); you route events
  yourself (no focus system).
- Official examples (hello_world, clock, text_box, ncdu, ...):
  https://github.com/janestreet/bonsai_term_examples/tree/with-extensions
  (note: code lives on the `with-extensions` branch; `master` is empty).
