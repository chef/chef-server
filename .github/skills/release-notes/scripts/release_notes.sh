#!/usr/bin/env bash
# End-to-end release notes draft for one product release.
#
# Usage:
#   release_notes.sh --from PREV_VERSION [--to VERSION] [--product chef-server]
#                    [--old-manifest FILE] [--new-manifest FILE]
#                    [--jira FILE] [--notes FILE] [--cve-extra FILE]
#                    [--github] [--date "October 20, 2026"] [--work-dir DIR]
#
# Reads (if present) grype results produced by the grype-scan skill:
#   dev-docs/security/grype/<PREV>/<name>.json and dev-docs/security/grype/<VERSION>/<name>.json
# and diffs every scan name present for both versions.
#
# Writes:
#   dev-docs/release-notes/<VERSION>.md     customer-facing draft (paste into the docs site)
#   <work-dir>/review.md                    internal review report (do not publish)
#   <work-dir>/*.json                       intermediate data for the agent
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SKILLS="$(cd "$SCRIPT_DIR/../.." && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
PRODUCT="chef-server"; FROM=""; TO=""; OLD_MANIFEST=""; NEW_MANIFEST=""
JIRA=""; NOTES=""; CVE_EXTRA=""; GITHUB=""; DATE=""; WORK=""

usage() { sed -n '2,19p' "$0" | sed 's/^# \{0,1\}//'; exit "${1:-0}"; }
while [[ $# -gt 0 ]]; do
  case "$1" in
    --product) PRODUCT="$2"; shift 2 ;;
    --from) FROM="$2"; shift 2 ;;
    --to) TO="$2"; shift 2 ;;
    --old-manifest) OLD_MANIFEST="$2"; shift 2 ;;
    --new-manifest) NEW_MANIFEST="$2"; shift 2 ;;
    --jira) JIRA="$2"; shift 2 ;;
    --notes) NOTES="$2"; shift 2 ;;
    --cve-extra) CVE_EXTRA="$2"; shift 2 ;;
    --github) GITHUB="--github"; shift ;;
    --date) DATE="$2"; shift 2 ;;
    --work-dir) WORK="$2"; shift 2 ;;
    -h|--help) usage 0 ;;
    *) echo "unknown option: $1" >&2; usage 1 ;;
  esac
done
[[ -n "$FROM" ]] || usage 1

cd "$REPO_ROOT"
CONFIG=".github/release-notes/products/$PRODUCT.json"
[[ -f "$CONFIG" ]] || { echo "error: no product config $CONFIG" >&2; exit 1; }
VERSION_FILE="$(python3 -c "import json;print(json.load(open('$CONFIG')).get('version_file','VERSION'))")"
TAG_FMT="$(python3 -c "import json;print(json.load(open('$CONFIG')).get('tag_format','{version}'))")"
GRYPE_DIR="$(python3 -c "import json;print(json.load(open('$CONFIG'))['security']['grype_dir'])")"
[[ -n "$TO" ]] || TO="$(tr -d '[:space:]' < "$VERSION_FILE")"
WORK="${WORK:-${TMPDIR:-/tmp}/release-notes-$PRODUCT-$TO}"
mkdir -p "$WORK" dev-docs/release-notes
OLD_TAG="${TAG_FMT//\{version\}/$FROM}"

echo "==> changes $FROM..$TO" >&2
python3 "$SKILLS/release-changes/scripts/release_changes.py" --product "$PRODUCT" \
  --from "$FROM" --to "$TO" $GITHUB ${JIRA:+--jira "$JIRA"} -o "$WORK/changes.json"

echo "==> component inventory" >&2
python3 "$SKILLS/component-inventory/scripts/component_inventory.py" --product "$PRODUCT" \
  ${NEW_MANIFEST:+--version-manifest "$NEW_MANIFEST"} -o "$WORK/inventory-new.json"
INV_DIFF_ARGS=()
if git rev-parse --verify --quiet "$OLD_TAG^{commit}" >/dev/null; then
  python3 "$SKILLS/component-inventory/scripts/component_inventory.py" --product "$PRODUCT" --ref "$OLD_TAG" \
    ${OLD_MANIFEST:+--version-manifest "$OLD_MANIFEST"} -o "$WORK/inventory-old.json"
  python3 "$SKILLS/component-inventory/scripts/component_inventory.py" \
    --diff "$WORK/inventory-old.json" "$WORK/inventory-new.json" -o "$WORK/inventory-diff.json"
  INV_DIFF_ARGS=(--inventory-diff "$WORK/inventory-diff.json")
else
  echo "warning: tag $OLD_TAG not available locally (try: git fetch --tags --unshallow); skipping Updated components" >&2
fi

echo "==> grype diffs" >&2
CVE_ARGS=()
if [[ -d "$GRYPE_DIR/$FROM" && -d "$GRYPE_DIR/$TO" ]]; then
  for new in "$GRYPE_DIR/$TO"/*.json; do
    name="$(basename "$new" .json)"
    [[ "$name" == diff-* || "$name" == *.raw ]] && continue
    old="$GRYPE_DIR/$FROM/$name.json"
    [[ -f "$old" ]] || { echo "warning: no $old to compare with $new" >&2; continue; }
    python3 "$SKILLS/grype-scan/scripts/grype_report.py" diff "$old" "$new" \
      -o "$GRYPE_DIR/$TO/diff-$name-from-$FROM.json" --markdown "$GRYPE_DIR/$TO/diff-$name-from-$FROM.md"
    CVE_ARGS+=(--cve-diff "$GRYPE_DIR/$TO/diff-$name-from-$FROM.json")
  done
else
  echo "warning: grype results for $FROM and/or $TO missing under $GRYPE_DIR; run the grype-scan skill first" >&2
fi

echo "==> render" >&2
python3 "$SKILLS/release-notes/scripts/render_release_notes.py" --product "$PRODUCT" \
  --changes "$WORK/changes.json" --inventory "$WORK/inventory-new.json" \
  "${INV_DIFF_ARGS[@]}" "${CVE_ARGS[@]}" \
  ${CVE_EXTRA:+--cve-extra "$CVE_EXTRA"} ${NOTES:+--notes "$NOTES"} ${DATE:+--date "$DATE"} \
  -o "dev-docs/release-notes/$TO.md" --review "$WORK/review.md"

echo "draft:  dev-docs/release-notes/$TO.md" >&2
echo "review: $WORK/review.md" >&2
