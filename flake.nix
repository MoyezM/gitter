{
  # Self-contained dev environment for gitter.
  #
  #   nix develop          # or let direnv load it via .envrc
  #   scripts/bootstrap    # first time only: builds the OxCaml opam switch
  #   dune build
  #
  # Layering (deliberate): nix owns every SYSTEM dependency (C libraries,
  # pkg-config, the opam tool, helpers the scripts/tests use); opam owns the
  # OCaml toolchain and packages, because the compiler is a custom OxCaml
  # switch (5.2.0+ox) from the oxcaml opam repository with preview-versioned
  # Jane Street packages — exactly the case opam-nix handles poorly. dune
  # stays the build system and runs unchanged inside this shell.
  #
  # NOTE: flakes only see git-TRACKED files; until flake.nix is committed,
  # use `nix develop path:.` (the .envrc does this for you).
  description = "gitter dev environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      forSystems = f:
        nixpkgs.lib.genAttrs [ "aarch64-darwin" "x86_64-darwin" "x86_64-linux" "aarch64-linux" ]
          (system: f nixpkgs.legacyPackages.${system});
    in
    {
      devShells = forSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            # OCaml bootstrap: the tool only — the switch lives in ~/.opam
            opam
            # ocamlopt's configured assembler is literally "gcc"; inside the
            # shell /usr/bin/gcc is an xcrun shim that fails against the nix
            # SDK, so alias gcc to the shell's clang wrapper
            (pkgs.writeShellScriptBin "gcc" ''exec ${pkgs.stdenv.cc}/bin/cc "$@"'')
            # native deps of the opam packages (zarith needs gmp; the
            # tree-sitter opam package's build wants autoconf)
            pkg-config
            gmp
            autoconf
            # sqlite3 opam bindings (review-marks store)
            sqlite
            # (libvterm-neovim was here for the embedded terminal — re-add
            # it when reviving lib/terminal, see lib/terminal/README.md)
            # scripts/add-grammar and the test harnesses
            curl
            git
            python3
            # editor used by the pty integration tests
            vim
          ];

          shellHook = ''
            if opam env --switch=5.2.0+ox --set-switch >/dev/null 2>&1; then
              eval "$(opam env --switch=5.2.0+ox --set-switch)"
            else
              echo "gitter: OxCaml switch missing — run scripts/bootstrap (one-time, ~30min compiler build)"
            fi
          '';
        };
      });
    };
}
