#!/usr/bin/env python3
"""Collect and classify the changes (PRs + Jira keys) that went into a release.

Primary source is the Expeditor-maintained CHANGELOG.md; `--source git` falls back
to squash-merge commit subjects ("Title (#1234)") between two git refs.

Classification precedence (first match wins):
  1. excluded   - label / title pattern / PR touches only non-shipping paths
  2. Jira type  - from --jira JSON (fetch with the Jira MCP server and save to a file)
  3. PR labels  - classification.label_sections in the product config
  4. dependency - PR touches a dependency file (lockfiles, overrides, plan.sh)
  5. title keyword regexes
  6. "unclassified" - the agent must decide and is told so in the review report

Examples:
  release_changes.py --product chef-server --from 15.10.125 --to 15.10.127
  release_changes.py --from 15.10.125 --to 15.10.127 --github --jira /tmp/jira.json -o /tmp/changes.json
"""
import argparse
import json
import os
import re
import subprocess
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "..", "..", "release-notes", "lib"))
import relnotes  # noqa: E402

RELEASE_HEADING = re.compile(r"^## \[(?P<version>[^\]]+)\]\([^)]*\)(?:\s*\((?P<date>[\d-]+)\))?", re.M)
PR_LINE = re.compile(
    r"^- (?P<title>.+?) \[#(?P<number>\d+)\]\((?P<url>[^)]+)\)(?: \(\[(?P<author>[^\]]+)\]\([^)]*\)\))?",
    re.M)
ROLLUP_LINE = re.compile(r"^(?P<line>- .+?) <!-- (?P<version>[\w.\-]+) -->\s*$", re.M)
SQUASH_SUBJECT = re.compile(r"^(?P<title>.+?) \(#(?P<number>\d+)\)$")
CVE = re.compile(r"\b(CVE-\d{4}-\d{4,}|GHSA-[\w]{4}-[\w]{4}-[\w]{4})\b", re.I)
RELEASE_NOTE_SECTION = re.compile(r"^#{2,4}\s*Release Notes?\s*$(?P<body>.*?)(?=^#{1,4}\s|\Z)", re.M | re.S | re.I)
TEMPLATE_PLACEHOLDER = re.compile(r"^\[.*\]$", re.S)
HTML_ENTITIES = {"&gt;": ">", "&lt;": "<", "&amp;": "&", "&#39;": "'", "&quot;": '"'}


def unescape(text):
    for k, v in HTML_ENTITIES.items():
        text = text.replace(k, v)
    return text


def parse_changelog(text, from_version, to_version):
    """Return (releases, prs) for releases where from_version < version <= to_version."""
    lo, hi = relnotes.version_key(from_version), relnotes.version_key(to_version)
    headings = list(RELEASE_HEADING.finditer(text))
    releases, prs, seen = [], [], set()
    for i, h in enumerate(headings):
        ver = h.group("version")
        if not (lo < relnotes.version_key(ver) <= hi):
            continue
        end = headings[i + 1].start() if i + 1 < len(headings) else len(text)
        body = text[h.end():end]
        # Stop at Expeditor marker comments that start the next block (rollups etc.)
        body = body.split("<!-- release_rollup")[0]
        releases.append({"version": ver, "date": h.group("date")})
        for m in PR_LINE.finditer(body):
            num = int(m.group("number"))
            if num in seen:
                continue
            seen.add(num)
            prs.append({"number": num, "title": unescape(m.group("title")).strip(),
                        "url": m.group("url"), "author": m.group("author"), "release": ver})
    # Expeditor only writes a "## [x.y.z]" heading for builds that were promoted; the
    # release_rollup block tags every PR line with "<!-- x.y.z -->", so use it to fill gaps.
    for m in ROLLUP_LINE.finditer(text):
        ver, pr = m.group("version"), PR_LINE.match(m.group("line"))
        if not pr or not (lo < relnotes.version_key(ver) <= hi):
            continue
        num = int(pr.group("number"))
        if num in seen:
            continue
        seen.add(num)
        prs.append({"number": num, "title": unescape(pr.group("title")).strip(),
                    "url": pr.group("url"), "author": pr.group("author"), "release": ver})
    return releases, prs


def parse_git(from_ref, to_ref, repo_url):
    root = relnotes.repo_root()
    for ref in (from_ref, to_ref):
        if not relnotes.git_ref_exists(ref, root):
            sys.exit(f"error: git ref '{ref}' not found; run `git fetch --tags --unshallow` first.")
    out = subprocess.run(["git", "-C", root, "log", "--format=%s", f"{from_ref}..{to_ref}"],
                         capture_output=True, text=True, check=True).stdout
    prs = []
    for line in out.splitlines():
        m = SQUASH_SUBJECT.match(line.strip())
        if m:
            n = int(m.group("number"))
            prs.append({"number": n, "title": m.group("title"), "url": f"{repo_url}/pull/{n}",
                        "author": None, "release": to_ref})
    return [{"version": to_ref, "date": None}], prs


def enrich_from_github(prs, repo):
    """Add labels, changed files and body via the gh CLI (needs GH_TOKEN)."""
    for pr in prs:
        proc = subprocess.run(["gh", "pr", "view", str(pr["number"]), "--repo", repo, "--json",
                               "labels,files,body,mergedAt,headRefName"],
                              capture_output=True, text=True)
        if proc.returncode != 0:
            pr.setdefault("warnings", []).append(f"gh pr view failed: {proc.stderr.strip()[:200]}")
            continue
        data = json.loads(proc.stdout)
        pr["labels"] = [lbl["name"] for lbl in data.get("labels", [])]
        pr["files"] = [f["path"] for f in data.get("files", [])]
        pr["body"] = data.get("body") or ""
        pr["merged_at"] = data.get("mergedAt")
        pr["branch"] = data.get("headRefName")


def extract_release_note(body):
    """Return the author's "### Release Note" text, "" when it says none, or None if absent/unfilled."""
    m = RELEASE_NOTE_SECTION.search(body or "")
    if not m:
        return None
    text = re.sub(r"<!--.*?-->", "", m.group("body"), flags=re.S).strip()
    if not text or TEMPLATE_PLACEHOLDER.match(text):
        return None
    if text.lower().rstrip(".") in ("none", "n/a", "na", "no"):
        return ""
    return " ".join(text.split())


def classify(pr, cfg, jira):
    c = cfg["classification"]
    labels = set(pr.get("labels", []))
    title = pr["title"]
    files = pr.get("files")

    if labels & set(c.get("exclude_labels", [])):
        return "excluded", "label"
    if pr.get("release_note") == "":
        return "excluded", "PR release note says none"
    for pat in c.get("exclude_title_patterns", []):
        if re.search(pat, title):
            return "excluded", f"title matches {pat}"
    if files:
        only = c.get("exclude_path_globs_if_only", [])
        if only and all(relnotes.any_glob(f, only) for f in files):
            return "excluded", "only non-shipping paths changed"

    for key in pr.get("jira", []):
        issue = jira.get(key) or {}
        section = c.get("jira_type_sections", {}).get(issue.get("type", ""))
        if section:
            if section != "security" and pr.get("cves"):
                return "security", f"Jira {key} is {issue['type']} but PR cites CVEs"
            return section, f"Jira {key} type {issue['type']}"

    for label, section in c.get("label_sections", {}).items():
        if label in labels:
            return section, f"label {label}"

    if pr.get("cves"):
        return "security", "CVE referenced"
    if files and any(relnotes.any_glob(f, c.get("dependency_files", [])) for f in files):
        return "dependency", "dependency file changed"
    for section, pat in c.get("title_keyword_sections", {}).items():
        if re.search(pat, title):
            return section, f"title keyword ({section})"
    return "unclassified", "no rule matched"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--product", default="chef-server")
    ap.add_argument("--from", dest="from_version", required=True, help="previous release (exclusive)")
    ap.add_argument("--to", dest="to_version", help="this release (inclusive; default: VERSION file)")
    ap.add_argument("--source", choices=["changelog", "git"], default="changelog")
    ap.add_argument("--github", action="store_true", help="enrich PRs via `gh pr view`")
    ap.add_argument("--jira", help="JSON file {KEY: {type, summary, status, customer_facing?}}")
    ap.add_argument("-o", "--output", default="-")
    args = ap.parse_args()

    cfg = relnotes.load_product(args.product)
    to_version = args.to_version or (relnotes.read_at_ref(cfg.get("version_file", "VERSION")) or "").strip()
    repo_url = f"https://github.com/{cfg['github_repo']}"

    if args.source == "changelog":
        text = relnotes.read_at_ref(cfg["changelog"]["path"])
        if text is None:
            sys.exit(f"error: {cfg['changelog']['path']} not found")
        releases, prs = parse_changelog(text, args.from_version, to_version)
    else:
        fmt = cfg.get("tag_format", "{version}")
        releases, prs = parse_git(fmt.format(version=args.from_version),
                                  fmt.format(version=to_version), repo_url)

    if args.github:
        enrich_from_github(prs, cfg["github_repo"])

    jira = relnotes.read_json(args.jira) if args.jira else {}
    key_re = re.compile(cfg["jira"]["key_pattern"])
    for pr in prs:
        haystack = " ".join([pr["title"], pr.get("branch") or "", pr.get("body") or ""])
        pr["jira"] = sorted(set(key_re.findall(haystack)), key=relnotes.version_key)
        pr["cves"] = sorted({m.upper() for m in CVE.findall(haystack)})
        note = extract_release_note(pr.get("body"))
        if note is not None:
            pr["release_note"] = note
        pr["section"], pr["reason"] = classify(pr, cfg, jira)

    sections = {}
    for pr in prs:
        sections.setdefault(pr["section"], []).append(pr["number"])
    relnotes.write_json({
        "product": cfg["product"],
        "from": args.from_version,
        "to": to_version,
        "releases": releases,
        "enriched_from_github": args.github,
        "summary": {k: len(v) for k, v in sorted(sections.items())},
        "prs": prs,
    }, args.output)


if __name__ == "__main__":
    main()
