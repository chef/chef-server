#!/usr/bin/env python3
"""Build (and optionally diff) the major third-party component inventory for a product.

Examples:
  # Inventory of the working tree
  component_inventory.py --product chef-server

  # Inventory at the previous release tag, using that build's omnibus version-manifest
  component_inventory.py --product chef-server --ref 15.10.125 \
      --version-manifest /tmp/15.10.125-version-manifest.json -o /tmp/inv-old.json

  # Diff two inventories
  component_inventory.py --diff /tmp/inv-old.json /tmp/inv-new.json -o /tmp/inv-diff.json
"""
import argparse
import os
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "..", "..", "..", "release-notes", "lib"))
import relnotes  # noqa: E402

GEM_SPEC = re.compile(r"^    ([A-Za-z0-9_.\-]+) \(([^)]+)\)$", re.M)
REBAR_ENTRY = re.compile(
    r"\{<<\"(?P<name>[^\"]+)\">>,\s*"
    r"(?:\{pkg,<<\"[^\"]+\">>,<<\"(?P<pkg>[^\"]+)\">>"
    r"|\{git,\"(?P<url>[^\"]+)\",\s*\{(?P<kind>ref|tag|branch),\s*\"(?P<val>[^\"]+)\"\}\})",
    re.S)


def parse_gemfile_lock(text):
    """Return {gem: version} for top-level specs (4-space indent) in a Gemfile.lock."""
    gems = {}
    for name, ver in GEM_SPEC.findall(text or ""):
        # Strip platform suffixes such as 1.18.9-x86_64-linux-gnu
        ver = re.sub(r"-(x86_64|aarch64|arm64|x64|java|universal)[\w\-]*$", "", ver)
        gems.setdefault(name, ver)
    return gems


def parse_rebar_lock(text):
    """Return {dep: version-string} from a rebar.lock (hex pkg version, tag, branch or ref)."""
    deps = {}
    for m in REBAR_ENTRY.finditer(text or ""):
        if m.group("pkg"):
            ver = m.group("pkg")
        elif m.group("kind") == "ref":
            ver = f"git {m.group('val')[:12]}"
        else:
            ver = f"{m.group('kind')} {m.group('val')}"
        deps[m.group("name")] = ver
    return deps


def manifest_version(manifest, keys):
    software = (manifest or {}).get("software", {})
    for key in keys:
        entry = software.get(key)
        if entry:
            ver = entry.get("described_version") or entry.get("locked_version")
            if ver:
                return ver, f"version-manifest.json:{key}"
    return None, None


def resolve(component, ref, manifest, cache):
    """Try each source in order; first hit wins. Returns (version, source, details)."""
    for src in component.get("sources", []):
        kind = src["type"]
        if kind == "version_manifest":
            ver, where = manifest_version(manifest, src["keys"])
            if ver:
                return ver, where, None
        elif kind == "regex":
            text = cache_read(cache, src["file"], ref)
            m = re.search(src["pattern"], text or "", re.M)
            if m:
                return m.group(1), src["file"], None
        elif kind in ("gemfile_lock", "rebar_lock"):
            parser = parse_gemfile_lock if kind == "gemfile_lock" else parse_rebar_lock
            key = src["gem"] if kind == "gemfile_lock" else src["dep"]
            per_file = {}
            for f in src["files"]:
                parsed = cache.setdefault(("parsed", f, ref), parser(cache_read(cache, f, ref)))
                if key in parsed:
                    per_file[f] = parsed[key]
            if per_file:
                versions = sorted(set(per_file.values()), key=relnotes.version_key)
                details = per_file if len(versions) > 1 else None
                return " / ".join(versions), ", ".join(per_file), details
        else:
            sys.exit(f"error: unknown source type '{kind}' in component {component['name']}")
    return None, None, None


def cache_read(cache, path, ref):
    k = ("file", path, ref)
    if k not in cache:
        cache[k] = relnotes.read_at_ref(path, ref)
    return cache[k]


def build_inventory(product, ref=None, manifest=None):
    cache = {}
    components = []
    for comp in product["components"]:
        ver, where, details = resolve(comp, ref, manifest, cache)
        entry = {"name": comp["name"], "tier": comp["tier"], "version": ver, "source": where}
        if details:
            entry["per_file"] = details
        if comp.get("advisories"):
            entry["advisories"] = comp["advisories"]
        components.append(entry)
    return {
        "product": product["product"],
        "ref": ref or "WORKTREE",
        "version": (relnotes.read_at_ref(product.get("version_file", "VERSION"), ref) or "").strip(),
        "version_manifest": bool(manifest),
        "components": components,
    }


def diff_inventories(old, new):
    old_map = {c["name"]: c for c in old["components"]}
    changes = []
    for c in new["components"]:
        o = old_map.get(c["name"], {})
        if o.get("version") != c.get("version"):
            changes.append({"name": c["name"], "tier": c["tier"],
                            "previous": o.get("version"), "new": c.get("version")})
    new_names = {c["name"] for c in new["components"]}
    for name, o in old_map.items():
        if name not in new_names:
            changes.append({"name": name, "tier": o["tier"], "previous": o.get("version"),
                            "new": None})
    return {"from": old.get("version") or old.get("ref"), "to": new.get("version") or new.get("ref"),
            "changes": changes}


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--product", default="chef-server")
    ap.add_argument("--ref", help="git ref to read files at (default: working tree)")
    ap.add_argument("--version-manifest", help="omnibus /opt/<pkg>/version-manifest.json for this build")
    ap.add_argument("--set", action="append", default=[], metavar="NAME=VERSION",
                    help="override/fill a component version (e.g. vendor builds: 'OpenSearch=1.3.20-tuxcare-1.0.3')")
    ap.add_argument("--diff", nargs=2, metavar=("OLD", "NEW"), help="diff two inventory JSON files")
    ap.add_argument("-o", "--output", default="-")
    args = ap.parse_args()

    if args.diff:
        relnotes.write_json(diff_inventories(relnotes.read_json(args.diff[0]),
                                             relnotes.read_json(args.diff[1])), args.output)
        return
    product = relnotes.load_product(args.product)
    manifest = relnotes.read_json(args.version_manifest) if args.version_manifest else None
    inv = build_inventory(product, args.ref, manifest)
    by_name = {c["name"]: c for c in inv["components"]}
    for item in args.set:
        name, _, ver = item.partition("=")
        if name not in by_name or not ver:
            sys.exit(f"error: --set {item!r}: expected NAME=VERSION with NAME one of: {', '.join(by_name)}")
        by_name[name].update(version=ver, source="--set")
    missing = [c["name"] for c in inv["components"] if not c["version"]]
    if missing:
        hint = "" if manifest else " (pass --version-manifest from a built package to resolve bundled services)"
        print(f"warning: no version found for: {', '.join(missing)}{hint}", file=sys.stderr)
    relnotes.write_json(inv, args.output)


if __name__ == "__main__":
    main()
