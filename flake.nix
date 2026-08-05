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

      # Distribution: the released binary must not reference /nix/store
      # dylibs, so libvterm and sqlite link STATICALLY. nixpkgs'
      # libvterm-neovim builds shared-only (libtool, no .a) — compile the
      # archive ourselves; the source ships its generated .inc tables, so
      # it is just cc + ar. sqlite's nix output already contains the .a.
      libvterm-static = pkgs:
        pkgs.stdenv.mkDerivation {
          pname = "libvterm-neovim-static";
          version = pkgs.libvterm-neovim.version;
          src = pkgs.libvterm-neovim.src;
          dontConfigure = true;
          buildPhase = ''
            cc -O2 -std=c99 -Iinclude -Isrc -c src/*.c
            ar rcs libvterm.a *.o
          '';
          installPhase = ''
            mkdir -p $out/lib
            cp libvterm.a $out/lib/
          '';
        };

      # A directory holding ONLY the static archives. Prepended to the
      # link path: macOS ld prefers a dylib over a .a only WITHIN a
      # directory, so an archive-only dir that comes first pins both
      # libraries to their static versions (bin/dune consumes this via
      # $GITTER_STATIC_LIB_DIR; -dead_strip_dylibs drops the now-unused
      # dylib references the opam libs still mention).
      static-libs = pkgs:
        pkgs.linkFarm "gitter-static-libs" [
          { name = "lib/libvterm.a"; path = "${libvterm-static pkgs}/lib/libvterm.a"; }
          { name = "lib/libsqlite3.a"; path = "${pkgs.sqlite.out}/lib/libsqlite3.a"; }
        ];
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
            # embedded-terminal emulator (the shell overlay, lib/terminal/)
            libvterm-neovim
            # scripts/add-grammar and the test harnesses
            curl
            git
            python3
            # editor used by the pty integration tests
            vim
          ];

          shellHook = ''
            export GITTER_STATIC_LIB_DIR=${static-libs pkgs}/lib
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
