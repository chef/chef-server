#!/usr/bin/env python3
"""Render customer-facing release notes (Markdown) plus an internal review report.

All inputs except --changes are optional; any section without data renders as
"None in this release." so every release has the same, searchable layout.

Inputs (produced by the sibling skills):
  --changes         release_changes.py output
  --inventory       component_inventory.py output for THIS build (Service versions)
  --inventory-diff  component_inventory.py --diff output (Updated components)
  --cve-diff        grype_report.py diff output (repeatable; one per scan target)
  --cve-extra       vendor-asserted fixes grype cannot see, JSON list of
                    {"id", "component", "severity"?, "fixed_in"?, "previous"?, "reference"?}
  --notes           agent-written customer wording, JSON:
                    {"highlights": "...", "upgrade_notes": ["..."], "known_issues": ["..."],
                     "hardening": ["..."], "prs": {"4239": {"text": "...", "section": "bug_fixes"}}}
"""
import argparse
import datetime
import os
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "..", "..", "release-notes", "lib"))
import relnotes  # noqa: E402

SECTIONS = [
    ("upgrade_notes", "Upgrade notes and breaking changes"),
    ("features", "New features"),
    ("improvements", "Improvements"),
    ("bug_fixes", "Bug fixes"),
]
SEVERITY_ORDER = ["Critical", "High", "Medium", "Low", "Negligible", "Unknown"]
NONE = "None in this release."
LEADING_KEY = re.compile(r"^\s*\[?([A-Z][A-Z0-9]+-\d+)\]?\s*[:\-–]?\s*")


def clean_title(title):
    t = LEADING_KEY.sub("", title).strip()
    return t[:1].upper() + t[1:] if t else title


def ref_links(pr, cfg):
    parts = [f"[#{pr['number']}]({pr['url']})"]
    base = cfg.get("jira", {}).get("base_url")
    for key in pr.get("jira", []):
        parts.append(f"[{key}]({base}{key})" if base else key)
    return ", ".join(parts)


def sev_rank(sev):
    return SEVERITY_ORDER.index(sev) if sev in SEVERITY_ORDER else len(SEVERITY_ORDER)


def cve_link(vid):
    if vid.startswith("CVE-"):
        return f"[{vid}](https://nvd.nist.gov/vuln/detail/{vid})"
    if vid.startswith("GHSA-"):
        return f"[{vid}](https://github.com/advisories/{vid})"
    return vid


def match_pr_for(text_candidates, prs, cve=None):
    """Find the PR that most likely delivered a dependency/CVE change."""
    if cve:
        for pr in prs:
            if cve in pr.get("cves", []):
                return pr
    for cand in text_candidates:
        if not cand:
            continue
        word = re.escape(cand.split(" (")[0].lower())
        pat = re.compile(rf"(?<![\w-]){word}(?![\w-])")
        for pr in prs:
            hay = " ".join([pr["title"].lower()] + [f.lower() for f in pr.get("files", [])])
            if pat.search(hay):
                return pr
    return None


def inventory_version(inventory, package):
    """Best-effort lookup of the shipped version of a package grype no longer flags."""
    for c in (inventory or {}).get("components", []):
        if c["name"].split(" (")[0].lower() == (package or "").lower() and c.get("version"):
            return c["version"]
    return None


def build_cve_rows(cve_diffs, extras, prs, cfg, review, inventory=None):
    rows = {}
    for d in cve_diffs:
        for f in d.get("fixed", []):
            key = (f["id"], f["package"])
            pr = match_pr_for([f["package"]], prs, f["id"])
            rows.setdefault(key, {
                "id": f["id"], "severity": f["severity"], "component": f["package"],
                "change": f"{f.get('previous_version') or '?'} → "
                          f"{f.get('new_version') or inventory_version(inventory, f['package']) or 'updated'}",
                "reference": ref_links(pr, cfg) if pr else "", "source": "grype"})
            if not pr:
                review["cves_without_pr"].append(f["id"])
    for e in extras:
        key = (e["id"], e["component"])
        pr = match_pr_for([e["component"]], prs, e["id"])
        change = e.get("fixed_in", "")
        if e.get("previous"):
            change = f"{e['previous']} → {change}"
        rows.setdefault(key, {
            "id": e["id"], "severity": e.get("severity", "Unknown"), "component": e["component"],
            "change": change, "reference": e.get("reference") or (ref_links(pr, cfg) if pr else ""),
            "source": "vendor-asserted"})
        review["vendor_asserted"].append(f"{e['id']} ({e['component']})")
    return sorted(rows.values(), key=lambda r: (r["component"].lower(), sev_rank(r["severity"]), r["id"]))


def render(cfg, changes, inventory, inv_diff, cve_diffs, extras, notes, release_date):
    review = {"unclassified": [], "excluded": [], "dependency_unmatched": [], "cves_without_pr": [],
              "vendor_asserted": [], "missing_versions": [], "inconsistencies": []}
    prs = changes.get("prs", [])
    pr_notes = {str(k): v for k, v in (notes.get("prs") or {}).items()}
    version = changes.get("to") or (inventory or {}).get("version")

    buckets = {k: [] for k, _ in SECTIONS}
    hardening = list(notes.get("hardening") or [])
    dep_prs = []
    for pr in prs:
        override = pr_notes.get(str(pr["number"]), {})
        section = override.get("section") or pr["section"]
        text = (override.get("text") or clean_title(pr["title"])).rstrip(". ")
        bullet = f"- {text} ({ref_links(pr, cfg)})"
        if section in buckets:
            buckets[section].append(bullet)
        elif section == "security":
            if not pr.get("cves"):
                hardening.append(f"{text} ({ref_links(pr, cfg)})")
        elif section == "dependency":
            dep_prs.append(pr)
        elif section == "excluded":
            review["excluded"].append(f"#{pr['number']} {pr['title']} — {pr.get('reason')}")
        else:
            review["unclassified"].append(f"#{pr['number']} {pr['title']}")
            buckets["improvements"].append(bullet + " <!-- REVIEW: unclassified -->")
    for item in notes.get("upgrade_notes") or []:
        buckets["upgrade_notes"].insert(0, f"- {item}")

    cve_rows = build_cve_rows(cve_diffs, extras, prs, cfg, review, inventory)
    if not cve_diffs:
        review["inconsistencies"].append("No grype diff supplied; CVE table built only from --cve-extra.")
    cve_components = {r["component"].lower() for r in cve_rows}

    out = [cfg["docs"]["heading_format"].format(display_name=cfg["display_name"], version=version), ""]
    out += [f"Release date: {release_date}", ""]
    prev = changes.get("from")
    if notes.get("highlights"):
        out += [notes["highlights"].strip(), ""]
    if prev:
        out += [f"This release includes all changes since {cfg['display_name']} {prev}.", ""]

    for key, title in SECTIONS:
        out += [f"### {title}", ""] + (buckets[key] or [NONE]) + [""]

    out += ["### Security fixes", ""]
    if cve_rows:
        out += [f"If a vulnerability scanner reported any of the following against {cfg['display_name']} "
                f"{prev or 'an earlier version'}, they're resolved in {version}.", "",
                "| CVE | Severity | Component | Version change | Reference |",
                "|-----|----------|-----------|----------------|-----------|"]
        out += [f"| {cve_link(r['id'])} | {r['severity']} | {r['component']} | {r['change']} | {r['reference']} |"
                for r in cve_rows]
        out.append("")
    if hardening:
        out += ["Other security improvements:", ""] + [f"- {h}" for h in hardening] + [""]
    if not cve_rows and not hardening:
        out += [NONE, ""]

    out += ["### Updated components", ""]
    changed = [c for c in (inv_diff or {}).get("changes", []) if c.get("previous") or c.get("new")]
    if changed:
        out += ["| Component | Previous | New | Reason | Reference |",
                "|-----------|----------|-----|--------|-----------|"]
        matched = set()
        for c in sorted(changed, key=lambda c: (c["tier"], c["name"].lower())):
            pr = match_pr_for([c["name"]], dep_prs + prs)
            if pr:
                matched.add(pr["number"])
            reason = "Security" if c["name"].split(" (")[0].lower() in cve_components else "Maintenance"
            out.append(f"| {c['name']} | {c.get('previous') or '—'} | {c.get('new') or 'removed'} | {reason} "
                       f"| {ref_links(pr, cfg) if pr else ''} |")
        out.append("")
        review["dependency_unmatched"] = [f"#{p['number']} {p['title']}" for p in dep_prs
                                          if p["number"] not in matched]
    else:
        review["dependency_unmatched"] = [f"#{p['number']} {p['title']}" for p in dep_prs]
        if inv_diff is None:
            review["inconsistencies"].append("No inventory diff supplied; Updated components table omitted.")
    unmatched = [p for p in dep_prs if f"#{p['number']} {p['title']}" in review["dependency_unmatched"]]
    if unmatched:
        out += ["Other dependency updates:" if changed else "Dependency updates:", ""]
        for p in unmatched:
            override = pr_notes.get(str(p["number"]), {})
            text = (override.get("text") or clean_title(p["title"])).rstrip(". ")
            out.append(f"- {text} ({ref_links(p, cfg)})")
        out.append("")
    elif not changed:
        out += [NONE, ""]

    out += ["### Service versions", "", "This release uses:", ""]
    if inventory:
        tiers = cfg.get("tiers", {})
        for tier, meta in sorted(tiers.items()):
            if not meta.get("always_list"):
                continue
            comps = [c for c in inventory["components"] if c["tier"] == tier]
            review["missing_versions"] += [c["name"] for c in comps if not c.get("version")]
            listed = [f"- {c['name']} {c['version']}" for c in comps if c.get("version")]
            if listed:
                out += [f"**{meta['title']}**", ""] + listed + [""]
        if not inventory.get("version_manifest"):
            review["inconsistencies"].append(
                "Inventory built without --version-manifest; bundled service versions may be missing or "
                "taken from source pins rather than the shipped package.")
    else:
        out += ["<!-- REVIEW: no inventory supplied -->", ""]

    if inventory:
        inv_names = {c["name"].split(" (")[0].lower(): c for c in inventory["components"]}
        changed_names = {c["name"].split(" (")[0].lower() for c in changed}
        for r in cve_rows:
            comp = r["component"].split(" (")[0].lower()
            if comp in inv_names and comp not in changed_names:
                review["inconsistencies"].append(
                    f"{r['id']} fixed in {r['component']} but its version did not change in the inventory diff.")

    known = notes.get("known_issues") or []
    if known:
        out += ["### Known issues", ""] + [f"- {k}" for k in known] + [""]
    return "\n".join(out).rstrip() + "\n", review


def render_review(review, cve_diffs):
    out = ["# Release notes review report (internal — do not publish)", ""]
    labels = {
        "unclassified": "PRs the classifier could not place (shown under Improvements, marked REVIEW)",
        "dependency_unmatched": "Dependency PRs not linked to any Updated components row",
        "cves_without_pr": "Fixed CVEs with no matching PR",
        "vendor_asserted": "Vendor-asserted CVE fixes (not verified by grype)",
        "missing_versions": "Always-listed components with no resolved version",
        "inconsistencies": "Consistency warnings",
        "excluded": "Excluded PRs",
    }
    for key, title in labels.items():
        items = review.get(key) or []
        out += [f"## {title} ({len(items)})", ""] + ([f"- {i}" for i in items] or ["None."]) + [""]
    introduced = sum(len(d.get("introduced", [])) for d in cve_diffs)
    remaining = sum(len(d.get("still_present", [])) for d in cve_diffs)
    out += ["## Open vulnerabilities (never publish automatically)", "",
            f"- Introduced since previous release: {introduced}",
            f"- Still present from previous release: {remaining}",
            "- See dev-docs/security/grype/<version>/diff-*.md for details.", ""]
    return "\n".join(out)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--product", default="chef-server")
    ap.add_argument("--changes", required=True)
    ap.add_argument("--inventory")
    ap.add_argument("--inventory-diff")
    ap.add_argument("--cve-diff", action="append", default=[])
    ap.add_argument("--cve-extra")
    ap.add_argument("--notes")
    ap.add_argument("--date", help="release date (default: today, e.g. 'October 20, 2026')")
    ap.add_argument("-o", "--output", default="-")
    ap.add_argument("--review", help="write the internal review report here")
    args = ap.parse_args()

    cfg = relnotes.load_product(args.product)
    load = lambda p: relnotes.read_json(p) if p else None  # noqa: E731
    today = datetime.date.today()
    date = args.date or f"{today:%B} {today.day}, {today.year}"
    cve_diffs = [relnotes.read_json(p) for p in args.cve_diff]
    md, review = render(cfg, load(args.changes), load(args.inventory), load(args.inventory_diff),
                        cve_diffs, load(args.cve_extra) or [], load(args.notes) or {}, date)
    if args.output in ("-", None):
        sys.stdout.write(md)
    else:
        with open(args.output, "w", encoding="utf-8") as fh:
            fh.write(md)
    if args.review:
        with open(args.review, "w", encoding="utf-8") as fh:
            fh.write(render_review(review, cve_diffs))


if __name__ == "__main__":
    main()
