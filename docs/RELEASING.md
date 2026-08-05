# Releasing

Runbook for cutting a gitter release. Users can never build from source (custom
OxCaml compiler + Jane Street preview packages), so every release ships
prebuilt binaries.

## One-time setup

1. Push `main` to GitHub — CI starts working on the first push:

   ```sh
   git push origin main
   ```

2. Create the tap repo `MoyezM/homebrew-tap`. Brew taps are just git repos; an
   empty repo with a `Formula/` directory is enough. The release workflow
   commits `Formula/gitter.rb` into it.

3. Optional: create a fine-grained PAT with `contents: write` on
   `homebrew-tap` only, and add it as repo secret `HOMEBREW_TAP_TOKEN` on
   `MoyezM/gitter`. Without it the release still publishes — the tap bump is
   skipped and you update the formula by hand.

## Cutting a release

Tag and push. That's the whole flow:

```sh
git tag v0.X.Y
git push origin v0.X.Y
```

The release workflow (on `macos-15`) then:

1. Builds the binary with `GITTER_VERSION` set from the tag, so
   `gitter -version` reports `v0.X.Y`.
2. Runs the portability gate — the binary must link only system libraries (no
   nix store or opam paths), so it runs on machines without the toolchain.
3. Strips and ad-hoc signs it (macOS refuses to run stripped-but-unsigned
   arm64 binaries).
4. Packs `gitter-v0.X.Y-macos-arm64.tar.gz` (exactly: `gitter`, `LICENSE`,
   `README.md`) and `checksums.txt` (`shasum -a 256 *.tar.gz`).
5. Publishes a GitHub Release with both files attached.
6. Bumps `Formula/gitter.rb` in `MoyezM/homebrew-tap` to the new URL and
   sha256 (skipped without `HOMEBREW_TAP_TOKEN`).

Timing: ~10 min with a warm cache; ~40+ min cold, almost all of it building
the OxCaml opam switch.

## Verifying

On a clean machine (no dev toolchain):

```sh
brew install MoyezM/tap/gitter
gitter -version   # prints the tag, e.g. v0.1.0
```

## Known limitations

- Artifacts are macOS arm64 only today. The naming scheme
  (`gitter-${TAG}-macos-arm64.tar.gz`) already extends to `macos-x86_64` and
  `linux-x86_64`; Linux and Intel builds are future work.
- The version comes from the tag (`GITTER_VERSION` at build time). Local
  builds report `dev`.
