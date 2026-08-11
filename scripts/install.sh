#!/usr/bin/env bash
# Installs a prebuilt gitter release. Users can never build from source (the
# toolchain is a custom OxCaml compiler plus preview packages), so this and
# `brew install MoyezM/tap/gitter` are the supported paths:
#
#   curl -fsSL https://raw.githubusercontent.com/MoyezM/gitter/main/scripts/install.sh | bash
#
# Overrides:
#   GITTER_VERSION      release tag to install (e.g. v0.1.0); default: latest
#   GITTER_INSTALL_DIR  where the binary goes; default: ~/.local/bin
#
# Piped to sh, so: no $0, no prompts, no bash arrays — and everything runs
# from main() at the bottom so a truncated download parses but does nothing.
set -euo pipefail

REPO=MoyezM/gitter
RELEASES="https://github.com/$REPO/releases"

err() {
  printf 'error: %s\n' "$1" >&2
  shift
  [ $# -eq 0 ] || printf '  %s\n' "$@" >&2
  exit 1
}

main() {
  command -v curl >/dev/null 2>&1 \
    || err "curl is required to download gitter — install it and re-run"

  # macOS ships shasum (perl), linux ships sha256sum (coreutils); both print
  # "<hash>  <file>".
  if command -v shasum >/dev/null 2>&1; then
    sha256() { shasum -a 256 "$1"; }
  elif command -v sha256sum >/dev/null 2>&1; then
    sha256() { sha256sum "$1"; }
  else
    err "neither shasum nor sha256sum found — cannot verify the download"
  fi

  os=$(uname -s)
  arch=$(uname -m)
  # linux says aarch64 where macOS says arm64; one name in asset filenames.
  case "$arch" in aarch64) arch=arm64 ;; esac
  case "$os-$arch" in
    Darwin-arm64)  platform=macos-arm64 ;;
    Darwin-x86_64) platform=macos-x86_64 ;;
    Linux-x86_64)  platform=linux-x86_64 ;;
    *) err "unsupported platform: $os/$arch" \
           "prebuilt binaries are listed at $RELEASES" ;;
  esac

  tag=${GITTER_VERSION:-}
  if [ -z "$tag" ]; then
    api="https://api.github.com/repos/$REPO/releases/latest"
    json=$(curl -fsSL "$api") \
      || err "could not query $api" \
             "offline, or the GitHub API rate limit (60 req/hr unauthenticated)?" \
             "retry later, or pin a tag: GITTER_VERSION=v0.1.0"
    # tag_name without jq: first "tag_name": "..." in the response.
    tag=$(printf '%s\n' "$json" \
      | sed -n 's/.*"tag_name"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' \
      | head -n 1)
    [ -n "$tag" ] \
      || err "no tag_name in the GitHub API response — no releases yet?" \
             "see $RELEASES"
  fi

  tmp=$(mktemp -d)
  trap 'rm -rf "$tmp"' EXIT

  asset="gitter-$tag-$platform.tar.gz"
  base="$RELEASES/download/$tag"

  printf 'downloading %s\n' "$asset"
  curl -fsSL -o "$tmp/$asset" "$base/$asset" \
    || err "download failed: $base/$asset" \
           "either tag $tag does not exist, or there is no prebuilt binary" \
           "for $platform yet (macos-arm64 and linux-x86_64 are published)." \
           "see $RELEASES"

  curl -fsSL -o "$tmp/checksums.txt" "$base/checksums.txt" \
    || err "download failed: $base/checksums.txt" \
           "release $tag has no checksums — refusing to install unverified binaries"

  # checksums.txt lines are `<sha256>  <asset>`; match ours by exact filename.
  want=$(awk -v f="$asset" '$2 == f { print $1 }' "$tmp/checksums.txt")
  [ -n "$want" ] || err "no entry for $asset in checksums.txt"
  got=$(sha256 "$tmp/$asset" | awk '{ print $1 }')
  [ "$want" = "$got" ] \
    || err "checksum mismatch for $asset" \
           "expected: $want" \
           "     got: $got" \
           "the download is corrupt or tampered with — not installing"

  tar -xzf "$tmp/$asset" -C "$tmp"
  [ -f "$tmp/gitter" ] || err "no gitter binary inside $asset — bad release artifact?"

  dest=${GITTER_INSTALL_DIR:-$HOME/.local/bin}
  mkdir -p "$dest"
  # rm first: overwriting a running binary in place trips macOS's
  # code-signature cache (Killed: 9); a fresh inode does not.
  rm -f "$dest/gitter"
  cp "$tmp/gitter" "$dest/gitter"
  chmod +x "$dest/gitter"

  case ":$PATH:" in
    *":$dest:"*) ;;
    *)
      pathline="export PATH=\"$dest:\$PATH\""
      printf '\n%s is not on your PATH — add this to your shell profile:\n' "$dest"
      printf '  %s\n\n' "$pathline"
      ;;
  esac

  printf 'installed: %s/gitter (%s)\n' "$dest" "$("$dest/gitter" -version)"
}

main
