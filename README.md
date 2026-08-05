# gitter

A git TUI built with [bonsai_term](https://github.com/janestreet/bonsai_term)
(Jane Street's Bonsai for terminals). See `SPEC.md` for the product spec.

## Install

macOS (Apple Silicon):

```sh
brew install MoyezM/tap/gitter
```

or:

```sh
curl -fsSL https://raw.githubusercontent.com/MoyezM/gitter/main/scripts/install.sh | bash
```

or grab a tarball from the
[releases page](https://github.com/MoyezM/gitter/releases).

Prebuilt binaries only — building from source needs the OxCaml toolchain (see
[Toolchain](#toolchain)).

## Toolchain

bonsai_term requires the [OxCaml](https://oxcaml.org) compiler (it uses labeled
tuples and stack-allocation modes). It is installed in the `5.2.0+ox` opam
switch, created with:

```sh
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
opam install bonsai_term bonsai_term_components ocaml-lsp-server
```

System prerequisites (`autoconf`, `pkg-config`, `gmp`) are managed in the
devmachine-dotfiles nix config. The `.envrc` activates the switch via direnv
(`direnv allow` once), which also makes the editor pick up the OxCaml-built
`ocamllsp`. Without direnv, prefix commands with
`opam exec --switch=5.2.0+ox --`.

## Run

```sh
dune exec bin/main.exe                      # current app
dune exec .archived/bin/main.exe            # archived M0 mock UI
dune exec .archived/bin/terminal_demo.exe   # archived embedded-terminal demo (needs tmux)
```

## Layout

- `bin/` — the current app (fresh start; being rebuilt)
- `.archived/` — the M0 interactive mock (two-mode file tree, stack switcher,
  mouse support, resizable panels) plus the tmux embedded-terminal demo. Kept
  buildable as a reference; library name `gitter_archived`. The root `dune`
  file opts this dot-directory back into the build.
