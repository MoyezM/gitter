#!/usr/bin/env bash
# Render packaging/homebrew/gitter.rb.tmpl into a homebrew-tap checkout.
# Contract (CI depends on it): env vars TAG (git tag, e.g. v0.1.0),
# CHECKSUMS_FILE (lines of "<sha256>  <filename>"), TAP_DIR (checkout of
# MoyezM/homebrew-tap). Writes $TAP_DIR/Formula/gitter.rb; the caller commits.
set -euo pipefail

: "${TAG:?set TAG to the git tag, e.g. v0.1.0}"
: "${CHECKSUMS_FILE:?set CHECKSUMS_FILE to the path of checksums.txt}"
: "${TAP_DIR:?set TAP_DIR to a checkout of MoyezM/homebrew-tap}"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
template="$script_dir/../../packaging/homebrew/gitter.rb.tmpl"
if [[ ! -f "$template" ]]; then
  echo "error: template not found at $template" >&2
  exit 1
fi

artifact="gitter-$TAG-macos-arm64.tar.gz"
# Exact match on the filename column so a future artifact sharing a substring
# (e.g. a differently suffixed tarball) can never be picked up by accident.
sha_arm64="$(awk -v f="$artifact" '$2 == f { print $1 }' "$CHECKSUMS_FILE")"
if [[ -z "$sha_arm64" ]]; then
  echo "error: no sha256 for $artifact in $CHECKSUMS_FILE" >&2
  echo "checksums file contents:" >&2
  cat "$CHECKSUMS_FILE" >&2
  exit 1
fi

version="${TAG#v}"
formula="$TAP_DIR/Formula/gitter.rb"
mkdir -p "$TAP_DIR/Formula"

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT
# Plain / delimiter is safe: tags are vX.Y.Z-ish and the sha is hex, so the
# substitutions can't contain a slash (or &, sed's other special replacement).
sed \
  -e "s/@TAG@/$TAG/g" \
  -e "s/@VERSION@/$version/g" \
  -e "s/@SHA_ARM64@/$sha_arm64/g" \
  "$template" > "$tmp"

# Diff against the previous formula (or /dev/null on first release) so the CI
# log shows exactly what the tap commit will contain.
old="/dev/null"
[[ -f "$formula" ]] && old="$formula"
if diff -u --label "old/Formula/gitter.rb" --label "new/Formula/gitter.rb" "$old" "$tmp"; then
  echo "Formula/gitter.rb unchanged"
fi

# mktemp creates 0600; give the tap checkout a normal file mode.
chmod 644 "$tmp"
mv "$tmp" "$formula"
echo "wrote $formula (tag $TAG, version $version, sha256 $sha_arm64)"
