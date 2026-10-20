#!/usr/bin/env bash
# Run a grype scan and store normalized results under dev-docs/security/grype/<version>/.
#
# Usage:
#   grype_scan.sh [--version V] [--name NAME] [--out-dir DIR] [--keep-raw] [--install] TARGET
#
# TARGET is anything grype accepts:
#   dir:/opt/opscode            installed omnibus package (most complete view of what ships)
#   dir:.                       repo lockfiles (Gemfile.lock, rebar.lock) - no build needed
#   sbom:dev-docs/security/sbom/15.10.127/github.spdx.json
#   docker:chef/chef-server-erchef:latest
#   ./chef-server-core_15.10.127-1_amd64.deb   (file)
#
# Outputs (NAME defaults to a slug of TARGET):
#   <out-dir>/<version>/<NAME>.json   normalized findings (small, diffable; commit this)
#   <out-dir>/<version>/<NAME>.md     human-readable summary
#   <out-dir>/<version>/<NAME>.raw.json  only with --keep-raw (large; do not commit)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null || pwd)"
VERSION=""
NAME=""
OUT_DIR="$REPO_ROOT/dev-docs/security/grype"
KEEP_RAW=0
INSTALL=0

usage() { sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'; exit "${1:-0}"; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    --version) VERSION="$2"; shift 2 ;;
    --name) NAME="$2"; shift 2 ;;
    --out-dir) OUT_DIR="$2"; shift 2 ;;
    --keep-raw) KEEP_RAW=1; shift ;;
    --install) INSTALL=1; shift ;;
    -h|--help) usage 0 ;;
    -*) echo "unknown option: $1" >&2; usage 1 ;;
    *) TARGET="$1"; shift ;;
  esac
done
[[ -n "${TARGET:-}" ]] || usage 1

if ! command -v grype >/dev/null 2>&1; then
  if [[ $INSTALL -eq 1 ]]; then
    mkdir -p "$HOME/.local/bin"
    curl -sSfL https://raw.githubusercontent.com/anchore/grype/main/install.sh | sh -s -- -b "$HOME/.local/bin"
    export PATH="$HOME/.local/bin:$PATH"
  else
    echo "error: grype not found. Install it (https://github.com/anchore/grype#installation) or re-run with --install." >&2
    exit 2
  fi
fi

[[ -n "$VERSION" ]] || VERSION="$(tr -d '[:space:]' < "$REPO_ROOT/VERSION" 2>/dev/null || echo unversioned)"
if [[ -z "$NAME" ]]; then
  NAME="$(echo "$TARGET" | sed -E 's#^(dir|sbom|docker|registry|file|oci-dir|oci-archive):##; s#[^A-Za-z0-9._-]+#-#g; s#^-+|-+$##g')"
  [[ -n "$NAME" ]] || NAME="scan"
fi

DEST="$OUT_DIR/$VERSION"
mkdir -p "$DEST"
RAW="$(mktemp -t grype-XXXXXX.json)"
trap 'rm -f "$RAW"' EXIT

echo "grype: updating vulnerability DB" >&2
grype db update >&2 || echo "warning: grype db update failed; using cached DB" >&2

echo "grype: scanning $TARGET" >&2
# --add-cpes-if-none improves matching for omnibus-built C software that has no package metadata.
grype "$TARGET" --add-cpes-if-none -o json --file "$RAW"

python3 "$SCRIPT_DIR/grype_report.py" normalize "$RAW" --target "$TARGET" --version "$VERSION" -o "$DEST/$NAME.json"
python3 "$SCRIPT_DIR/grype_report.py" summary "$DEST/$NAME.json" -o "$DEST/$NAME.md"
if [[ $KEEP_RAW -eq 1 ]]; then cp "$RAW" "$DEST/$NAME.raw.json"; fi

echo "wrote $DEST/$NAME.json and $DEST/$NAME.md" >&2
