#!/usr/bin/env bash
# Collect an SBOM into dev-docs/security/sbom/<version>/.
#
# Usage:
#   sbom.sh github    [--version V] [--repo OWNER/REPO]
#       Export GitHub's dependency-graph SBOM (SPDX 2.3 JSON) via `gh api`.
#       Note: GitHub generates this from the default branch's manifests/lockfiles,
#       so run it right after the release commit lands.
#   sbom.sh blackduck [--version V] FILE
#       Import an SBOM exported from Black Duck (SPDX or CycloneDX JSON, optionally
#       zipped), e.g. the report produced by the CI `perform-blackduck-sca-scan` job
#       or "Project version > Reports > Create SBOM" in the Black Duck UI.
#   sbom.sh syft      [--version V] TARGET
#       Generate an SPDX SBOM locally with syft (e.g. dir:/opt/opscode).
#
# Output: dev-docs/security/sbom/<version>/<source>.<spdx|cyclonedx>.json
# Scan any of these with: grype_scan.sh sbom:<file>
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null || pwd)"
OUT_DIR="$REPO_ROOT/dev-docs/security/sbom"
VERSION=""
REPO=""

usage() { sed -n '2,19p' "$0" | sed 's/^# \{0,1\}//'; exit "${1:-0}"; }

[[ $# -ge 1 ]] || usage 1
MODE="$1"; shift
ARGS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --version) VERSION="$2"; shift 2 ;;
    --repo) REPO="$2"; shift 2 ;;
    --out-dir) OUT_DIR="$2"; shift 2 ;;
    -h|--help) usage 0 ;;
    *) ARGS+=("$1"); shift ;;
  esac
done

[[ -n "$VERSION" ]] || VERSION="$(tr -d '[:space:]' < "$REPO_ROOT/VERSION" 2>/dev/null || echo unversioned)"
DEST="$OUT_DIR/$VERSION"
mkdir -p "$DEST"

sbom_format() {
  python3 - "$1" <<'PY'
import json, sys
d = json.load(open(sys.argv[1]))
if "spdxVersion" in d:
    print("spdx")
elif d.get("bomFormat") == "CycloneDX":
    print("cyclonedx")
else:
    sys.exit("error: %s is not SPDX or CycloneDX JSON" % sys.argv[1])
PY
}

case "$MODE" in
  github)
    command -v gh >/dev/null || { echo "error: gh CLI required" >&2; exit 2; }
    if [[ -z "$REPO" ]]; then
      REPO="$(git -C "$REPO_ROOT" remote get-url origin | sed -E 's#(git@github.com:|https://github.com/)##; s#\.git$##')"
    fi
    TMP="$(mktemp)"; trap 'rm -f "$TMP"' EXIT
    gh api "repos/$REPO/dependency-graph/sbom" --jq '.sbom' > "$TMP"
    sbom_format "$TMP" >/dev/null
    mv "$TMP" "$DEST/github.spdx.json"
    echo "wrote $DEST/github.spdx.json" >&2
    ;;
  blackduck)
    [[ ${#ARGS[@]} -eq 1 ]] || usage 1
    SRC="${ARGS[0]}"
    WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
    if [[ "$SRC" == *.zip ]]; then
      unzip -q -o "$SRC" -d "$WORK"
      SRC="$(find "$WORK" -type f -name '*.json' | head -n1)"
      [[ -n "$SRC" ]] || { echo "error: no JSON SBOM inside zip" >&2; exit 1; }
    fi
    FMT="$(sbom_format "$SRC")"
    cp "$SRC" "$DEST/blackduck.$FMT.json"
    echo "wrote $DEST/blackduck.$FMT.json" >&2
    ;;
  syft)
    [[ ${#ARGS[@]} -eq 1 ]] || usage 1
    command -v syft >/dev/null || { echo "error: syft not found (https://github.com/anchore/syft#installation)" >&2; exit 2; }
    syft "${ARGS[0]}" -o "spdx-json=$DEST/syft.spdx.json"
    echo "wrote $DEST/syft.spdx.json" >&2
    ;;
  *) usage 1 ;;
esac
