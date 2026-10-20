#!/usr/bin/env python3
"""Normalize, summarize and diff grype scan results.

Subcommands:
  normalize RAW.json -o OUT.json [--target NAME --version V]
      Reduce raw `grype -o json` output to a small, stable, diff-friendly file.
  summary SCAN.json [-o OUT.md]
      Markdown summary (counts by severity + table) of one scan.
  diff OLD.json NEW.json [-o OUT.json] [--markdown OUT.md]
      CVEs fixed (in OLD, gone from NEW), introduced, and still present.

Inputs may be raw grype JSON or normalized files; both are detected automatically.
"""
import argparse
import datetime
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "..", "..", "release-notes", "lib"))
import relnotes  # noqa: E402

SEVERITY_ORDER = ["Critical", "High", "Medium", "Low", "Negligible", "Unknown"]
NVD = "https://nvd.nist.gov/vuln/detail/"
GHSA = "https://github.com/advisories/"


def sev_rank(sev):
    return SEVERITY_ORDER.index(sev) if sev in SEVERITY_ORDER else len(SEVERITY_ORDER)


def advisory_url(vid):
    if vid.startswith("CVE-"):
        return NVD + vid
    if vid.startswith("GHSA-"):
        return GHSA + vid
    return None


def normalize(raw, target=None, version=None):
    """Accept raw grype JSON or an already-normalized document."""
    if "findings" in raw:
        return raw
    findings = {}
    for m in raw.get("matches", []):
        vuln, art = m.get("vulnerability", {}), m.get("artifact", {})
        vid = vuln.get("id")
        # Prefer the CVE alias when grype matched on a GHSA/distro advisory id.
        cve_alias = next((r.get("id") for r in m.get("relatedVulnerabilities", [])
                          if str(r.get("id", "")).startswith("CVE-")), None)
        ids = sorted({i for i in (vid, cve_alias) if i})
        primary = cve_alias if cve_alias and not str(vid).startswith("CVE-") else vid
        key = (primary, art.get("name"), art.get("version"))
        f = findings.setdefault(key, {
            "id": primary,
            "aliases": [i for i in ids if i != primary],
            "severity": vuln.get("severity", "Unknown"),
            "package": art.get("name"),
            "version": art.get("version"),
            "type": art.get("type"),
            "fix_versions": vuln.get("fix", {}).get("versions", []),
            "fix_state": vuln.get("fix", {}).get("state"),
            "locations": [],
        })
        for loc in art.get("locations", []):
            p = loc.get("path")
            if p and p not in f["locations"]:
                f["locations"].append(p)
    items = sorted(findings.values(),
                   key=lambda f: (sev_rank(f["severity"]), f["package"] or "", f["id"] or ""))
    for f in items:
        f["locations"] = sorted(f["locations"])[:5]
    desc = raw.get("descriptor", {})
    src = raw.get("source", {})
    src_target = src.get("target")
    if not isinstance(src_target, str):
        src_target = (src_target or {}).get("userInput")
    return {
        "schema": "grype-normalized/v1",
        "target": target or src_target,
        "source_type": src.get("type"),
        "product_version": version,
        "scanned_at": desc.get("timestamp") or datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "grype_version": desc.get("version"),
        "db_built": (desc.get("db") or {}).get("built") or
                    ((desc.get("db") or {}).get("status") or {}).get("built"),
        "findings": items,
    }


def counts(findings):
    c = {s: 0 for s in SEVERITY_ORDER}
    for f in findings:
        c[f["severity"] if f["severity"] in c else "Unknown"] += 1
    return c


def md_link(vid):
    url = advisory_url(vid)
    return f"[{vid}]({url})" if url else vid


def md_table(findings, with_fix=True):
    head = "| Vulnerability | Severity | Package | Installed | Fixed in |" if with_fix else \
        "| Vulnerability | Severity | Package | Installed |"
    sep = "|---|---|---|---|---|" if with_fix else "|---|---|---|---|"
    rows = [head, sep]
    for f in findings:
        row = f"| {md_link(f['id'])} | {f['severity']} | {f['package']} ({f.get('type') or '?'}) | {f['version']} |"
        if with_fix:
            row += f" {', '.join(f.get('fix_versions') or []) or f.get('fix_state') or '—'} |"
        rows.append(row)
    return "\n".join(rows)


def summary_md(doc):
    c = counts(doc["findings"])
    lines = [f"# Grype scan: {doc.get('target') or 'unknown target'}", "",
             f"- Product version: {doc.get('product_version') or 'n/a'}",
             f"- Scanned: {doc.get('scanned_at')} (grype {doc.get('grype_version')}, DB {doc.get('db_built')})",
             "- Totals: " + ", ".join(f"{s} {n}" for s, n in c.items() if n) if doc["findings"]
             else "- Totals: no findings", ""]
    if doc["findings"]:
        lines += [md_table(doc["findings"]), ""]
    return "\n".join(lines)


def finding_key(f):
    return (f["id"], f["package"])


def diff(old, new):
    old_by = {}
    for f in old["findings"]:
        old_by.setdefault(finding_key(f), f)
    new_by = {}
    for f in new["findings"]:
        new_by.setdefault(finding_key(f), f)

    fixed = []
    for k, f in old_by.items():
        if k in new_by:
            continue
        # Report which version of the package now ships, if grype still sees it.
        now = sorted({n["version"] for n in new["findings"] if n["package"] == f["package"]})
        fixed.append(dict(f, previous_version=f["version"], new_version=", ".join(now) or None))
    introduced = [f for k, f in new_by.items() if k not in old_by]
    remaining = [f for k, f in new_by.items() if k in old_by]
    order = lambda f: (f["package"] or "", sev_rank(f["severity"]), f["id"])  # noqa: E731
    return {
        "schema": "grype-diff/v1",
        "from": old.get("product_version") or old.get("target"),
        "to": new.get("product_version") or new.get("target"),
        "fixed": sorted(fixed, key=order),
        "introduced": sorted(introduced, key=order),
        "still_present": sorted(remaining, key=order),
    }


def diff_md(d):
    out = [f"# Vulnerability changes: {d['from']} → {d['to']}", ""]
    out += [f"## Fixed ({len(d['fixed'])})", ""]
    if d["fixed"]:
        out += ["| Vulnerability | Severity | Package | Previous | Now |", "|---|---|---|---|---|"]
        for f in d["fixed"]:
            out.append(f"| {md_link(f['id'])} | {f['severity']} | {f['package']} | {f['previous_version']} "
                       f"| {f.get('new_version') or 'no longer flagged'} |")
    else:
        out.append("None.")
    out += ["", f"## Introduced ({len(d['introduced'])})", "",
            md_table(d["introduced"]) if d["introduced"] else "None.",
            "", f"## Still present ({len(d['still_present'])})", "",
            md_table(d["still_present"]) if d["still_present"] else "None.", ""]
    return "\n".join(out)


def write_text(text, path):
    if path in (None, "-"):
        sys.stdout.write(text)
        return
    os.makedirs(os.path.dirname(os.path.abspath(path)), exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(text)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    n = sub.add_parser("normalize")
    n.add_argument("raw")
    n.add_argument("--target")
    n.add_argument("--version")
    n.add_argument("-o", "--output", default="-")
    s = sub.add_parser("summary")
    s.add_argument("scan")
    s.add_argument("-o", "--output", default="-")
    d = sub.add_parser("diff")
    d.add_argument("old")
    d.add_argument("new")
    d.add_argument("-o", "--output", default="-")
    d.add_argument("--markdown")
    args = ap.parse_args()

    if args.cmd == "normalize":
        relnotes.write_json(normalize(relnotes.read_json(args.raw), args.target, args.version), args.output)
    elif args.cmd == "summary":
        write_text(summary_md(normalize(relnotes.read_json(args.scan))), args.output)
    else:
        result = diff(normalize(relnotes.read_json(args.old)), normalize(relnotes.read_json(args.new)))
        relnotes.write_json(result, args.output)
        if args.markdown:
            write_text(diff_md(result), args.markdown)


if __name__ == "__main__":
    main()
