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
        # CI shell: the minimum the cached-switch build path needs. No nix
        # C toolchain (mkShellNoCC) — runners use Apple clang, which
        # handles our C stubs against nix include paths fine; the stdenv
        # clang closure alone costs ~60s of substitution per run. No
        # python/vim/curl (pty harnesses don't run in CI).
        # (cold bootstrap runs in the FULL default shell — this one only
        # ever hosts warm builds, so no autoconf, and the runner has git)
        ci = pkgs.mkShellNoCC {
          packages = with pkgs; [ opam pkg-config gmp sqlite libvterm-neovim ];
          shellHook = ''
            export GITTER_STATIC_LIB_DIR=${static-libs pkgs}/lib
            if opam env --switch=5.2.0+ox --set-switch >/dev/null 2>&1; then
              eval "$(opam env --switch=5.2.0+ox --set-switch)"
            fi
          '';
        };
        # No nix C toolchain on Linux (mkShellNoCC), deliberately: whatever
        # compiles the switch's C runtime must be the same toolchain that
        # later LINKS the binary, and the release link happens in the #ci
        # shell against the distro's gcc. Build libasmrun.a against nix's
        # newer glibc instead and it calls __isoc23_strtol/__isoc23_sscanf,
        # which glibc 2.35 does not define — every link fails with undefined
        # references. macOS has no such split: nix clang and Apple clang both
        # target the one system libSystem, so it keeps its full stdenv.
        default = (if pkgs.stdenv.isDarwin then pkgs.mkShell else pkgs.mkShellNoCC) {
          packages = (with pkgs; [
            # OCaml bootstrap: the tool only — the switch lives in ~/.opam
            opam
            # native deps of the opam packages (zarith needs gmp; the
            # tree-sitter opam package's build wants autoconf)
            pkg-config
            gmp
            autoconf
            # oxcaml-compiler's `make install` stages the compiler with rsync.
            # macOS ships one in /usr/bin so this never surfaced there; a
            # minimal Linux has none and the compiler build dies at the very
            # last step (Makefile.common-ox: install, exit 127).
            rsync
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
            # scripts/demo/record — vhs drives the app through ttyd and
            # encodes with ffmpeg. Dev shell only: CI never records, and
            # this closure is far too heavy to make warm builds pay for it.
            vhs
            ttyd
            ffmpeg
          ])
          # ocamlopt's configured assembler is literally "gcc"; on macOS
          # /usr/bin/gcc is an xcrun shim that fails against the nix SDK, so
          # alias gcc to the shell's clang wrapper. Linux uses its own gcc,
          # which is the whole point of the mkShellNoCC above.
          ++ pkgs.lib.optional pkgs.stdenv.isDarwin
               (pkgs.writeShellScriptBin "gcc" ''exec ${pkgs.stdenv.cc}/bin/cc "$@"'');

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
