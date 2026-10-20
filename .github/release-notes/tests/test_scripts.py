"""Unit tests for the release-notes / security skill scripts (stdlib unittest)."""
import importlib.util
import os
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
GITHUB_DIR = os.path.dirname(os.path.dirname(HERE))
SKILLS = os.path.join(GITHUB_DIR, "skills")


def load(name, rel):
    spec = importlib.util.spec_from_file_location(name, os.path.join(SKILLS, rel))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


inventory = load("component_inventory", "component-inventory/scripts/component_inventory.py")
changes = load("release_changes", "release-changes/scripts/release_changes.py")
grype = load("grype_report", "grype-scan/scripts/grype_report.py")
render = load("render_release_notes", "release-notes/scripts/render_release_notes.py")
relnotes = inventory.relnotes

GEMFILE_LOCK = """GEM
  remote: https://rubygems.org/
  specs:
    nokogiri (1.18.9-x86_64-linux-gnu)
      racc (~> 1.4)
    rack (3.2.6)
    rails (7.2.3.1)
      actionpack (= 7.2.3.1)

PLATFORMS
  x86_64-linux
"""

REBAR_LOCK = """{"1.2.0",
[{<<"bcrypt">>,
  {git,"https://github.com/erlangpack/bcrypt",
       {ref,"564b39e2f86a0e3d8ca08dcd1aae924acffe1408"}},
  0},
 {<<"sqerl">>,
  {git,"https://github.com/chef/sqerl",
       {branch,"main"}},
  0},
 {<<"jsx">>,{pkg,<<"jsx">>,<<"3.1.0">>},1}]}.
"""

CHANGELOG = """# Changelog
<!-- latest_release 1.0.3 -->
## [1.0.3](https://github.com/o/r/tree/1.0.3) (2026-10-05)

#### Merged Pull Requests
- CHEF-2 - Fix crash in reconfigure [#12](https://github.com/o/r/pull/12) ([dev](https://github.com/dev))
<!-- latest_release -->

<!-- release_rollup since=1.0.0 -->
### Changes since 1.0.0 release

#### Merged Pull Requests
- CHEF-2 - Fix crash in reconfigure [#12](https://github.com/o/r/pull/12) ([dev](https://github.com/dev)) <!-- 1.0.3 -->
- Docs tweak [#11](https://github.com/o/r/pull/11) ([dev](https://github.com/dev)) <!-- 1.0.2 -->
<!-- release_rollup -->

<!-- latest_stable_release -->
## [1.0.1](https://github.com/o/r/tree/1.0.1) (2026-08-27)

#### Merged Pull Requests
- Bump rack to 3.2.6 &gt;= floor [#10](https://github.com/o/r/pull/10) ([dev](https://github.com/dev))
<!-- latest_stable_release -->

## [1.0.0](https://github.com/o/r/tree/1.0.0) (2026-06-25)

#### Merged Pull Requests
- Old change [#9](https://github.com/o/r/pull/9) ([dev](https://github.com/dev))
"""


def raw_match(vid, sev, pkg, ver, related=None, fix=None):
    m = {"vulnerability": {"id": vid, "severity": sev, "fix": {"versions": fix or [], "state": "fixed"}},
         "artifact": {"name": pkg, "version": ver, "type": "gem", "locations": [{"path": "/Gemfile.lock"}]}}
    if related:
        m["relatedVulnerabilities"] = [{"id": related}]
    return m


class ProductConfigTest(unittest.TestCase):
    def test_all_product_configs_are_valid(self):
        products = os.path.join(GITHUB_DIR, "release-notes", "products")
        for f in os.listdir(products):
            if not f.endswith(".json"):
                continue
            with self.subTest(product=f):
                cfg = relnotes.read_json(os.path.join(products, f))
                for key in ("product", "display_name", "github_repo", "classification", "components", "tiers"):
                    self.assertIn(key, cfg)
                names = [c["name"] for c in cfg["components"]]
                self.assertEqual(len(names), len(set(names)), "duplicate component names")
                for c in cfg["components"]:
                    self.assertIn(c["tier"], cfg["tiers"])
                    for s in c["sources"]:
                        self.assertIn(s["type"], ("version_manifest", "regex", "gemfile_lock", "rebar_lock"))


class InventoryTest(unittest.TestCase):
    def test_parse_gemfile_lock_strips_platform_and_ignores_nested(self):
        gems = inventory.parse_gemfile_lock(GEMFILE_LOCK)
        self.assertEqual(gems["nokogiri"], "1.18.9")
        self.assertEqual(gems["rails"], "7.2.3.1")
        self.assertNotIn("racc", gems)

    def test_parse_rebar_lock_handles_ref_branch_and_pkg(self):
        deps = inventory.parse_rebar_lock(REBAR_LOCK)
        self.assertEqual(deps["bcrypt"], "git 564b39e2f86a")
        self.assertEqual(deps["sqerl"], "branch main")
        self.assertEqual(deps["jsx"], "3.1.0")

    def test_version_manifest_preferred_over_later_sources(self):
        comp = {"name": "Ruby", "tier": "B", "sources": [
            {"type": "version_manifest", "keys": ["ruby"]},
            {"type": "regex", "file": "does-not-exist", "pattern": "(x)"}]}
        manifest = {"software": {"ruby": {"locked_version": "3.1.7"}}}
        self.assertEqual(inventory.resolve(comp, None, manifest, {})[0], "3.1.7")

    def test_diff_reports_changed_and_removed(self):
        old = {"version": "1", "components": [{"name": "A", "tier": "A", "version": "1"},
                                              {"name": "B", "tier": "B", "version": "2"},
                                              {"name": "C", "tier": "C", "version": "3"}]}
        new = {"version": "2", "components": [{"name": "A", "tier": "A", "version": "1.1"},
                                              {"name": "B", "tier": "B", "version": "2"}]}
        d = inventory.diff_inventories(old, new)
        self.assertEqual({(c["name"], c["previous"], c["new"]) for c in d["changes"]},
                         {("A", "1", "1.1"), ("C", "3", None)})


class ChangesTest(unittest.TestCase):
    def test_parse_changelog_range_and_rollup(self):
        releases, prs = changes.parse_changelog(CHANGELOG, "1.0.1", "1.0.3")
        self.assertEqual([r["version"] for r in releases], ["1.0.3"])
        self.assertEqual(sorted(p["number"] for p in prs), [11, 12])

    def test_parse_changelog_unescapes_html(self):
        _, prs = changes.parse_changelog(CHANGELOG, "1.0.0", "1.0.1")
        self.assertEqual(prs[0]["title"], "Bump rack to 3.2.6 >= floor")

    def test_extract_release_note(self):
        body = ("### Description\nstuff\n\n### Release Note\n\nFixed `reconfigure` failing.\n\n"
                "### Check List\n- [ ] x\n")
        self.assertEqual(changes.extract_release_note(body), "Fixed `reconfigure` failing.")
        self.assertEqual(changes.extract_release_note("### Release Note\nnone\n"), "")
        self.assertIsNone(changes.extract_release_note("### Release Note\n[One customer-facing sentence]\n"))
        self.assertIsNone(changes.extract_release_note("no section"))

    def test_classification_precedence(self):
        cfg = relnotes.load_product("chef-server")
        base = {"title": "CHEF-1 - Something", "labels": [], "files": [], "jira": [], "cves": []}
        self.assertEqual(changes.classify(dict(base, title="Updated NOTICE.TXT"), cfg, {})[0], "excluded")
        self.assertEqual(changes.classify(dict(base, files=["docs-chef-io/a.md"]), cfg, {})[0], "excluded")
        jira = {"CHEF-1": {"type": "Bug"}}
        self.assertEqual(changes.classify(dict(base, jira=["CHEF-1"], files=["src/a.erl"]), cfg, jira)[0],
                         "bug_fixes")
        self.assertEqual(changes.classify(dict(base, jira=["CHEF-1"], cves=["CVE-2026-1"], files=["x.rb"]),
                                          cfg, jira)[0], "security")
        self.assertEqual(changes.classify(dict(base, files=["src/oc-id/Gemfile.lock"]), cfg, {})[0],
                         "dependency")
        self.assertEqual(changes.classify(dict(base, labels=["Type: Feature"], files=["a.rb"]), cfg, {})[0],
                         "features")


class GrypeTest(unittest.TestCase):
    def setUp(self):
        self.old = grype.normalize({"matches": [
            raw_match("GHSA-xxxx-yyyy-zzzz", "High", "rack", "3.2.5", related="CVE-2026-1", fix=["3.2.6"]),
            raw_match("CVE-2026-2", "Medium", "openssl", "3.1.2"),
        ]}, version="1.0.1")
        self.new = grype.normalize({"matches": [
            raw_match("CVE-2026-2", "Medium", "openssl", "3.1.2"),
            raw_match("CVE-2026-3", "Critical", "rexml", "3.4.4"),
        ]}, version="1.0.3")

    def test_normalize_prefers_cve_alias(self):
        rack = next(f for f in self.old["findings"] if f["package"] == "rack")
        self.assertEqual(rack["id"], "CVE-2026-1")
        self.assertEqual(rack["aliases"], ["GHSA-xxxx-yyyy-zzzz"])

    def test_normalize_is_idempotent(self):
        self.assertIs(grype.normalize(self.old), self.old)

    def test_diff(self):
        d = grype.diff(self.old, self.new)
        self.assertEqual([f["id"] for f in d["fixed"]], ["CVE-2026-1"])
        self.assertEqual([f["id"] for f in d["introduced"]], ["CVE-2026-3"])
        self.assertEqual([f["id"] for f in d["still_present"]], ["CVE-2026-2"])
        self.assertIn("CVE-2026-1", grype.diff_md(d))


class RenderTest(unittest.TestCase):
    def test_render_has_all_sections_and_cve_table(self):
        cfg = relnotes.load_product("chef-server")
        chg = {"from": "1.0.1", "to": "1.0.3", "prs": [
            {"number": 12, "title": "CHEF-2 - fix crash in reconfigure", "url": "u12", "jira": ["CHEF-2"],
             "cves": [], "section": "bug_fixes"},
            {"number": 13, "title": "Bump rack to 3.2.6", "url": "u13", "jira": [], "cves": [],
             "section": "dependency", "files": ["src/oc-id/Gemfile.lock"]},
            {"number": 14, "title": "Mystery", "url": "u14", "jira": [], "cves": [], "section": "unclassified"},
        ]}
        inv = {"version": "1.0.3", "version_manifest": True, "components": [
            {"name": "OpenSearch", "tier": "A", "version": "1.3.20"},
            {"name": "Rack", "tier": "D", "version": "3.2.6"}]}
        inv_diff = {"changes": [{"name": "Rack", "tier": "D", "previous": "3.2.5", "new": "3.2.6"}]}
        cve = {"fixed": [{"id": "CVE-2026-1", "severity": "High", "package": "rack",
                          "previous_version": "3.2.5", "new_version": None}]}
        md, review = render.render(cfg, chg, inv, inv_diff, [cve], [], {}, "October 5, 2026")
        for heading in ("### Upgrade notes and breaking changes", "### New features", "### Improvements",
                        "### Bug fixes", "### Security fixes", "### Updated components", "### Service versions"):
            self.assertIn(heading, md)
        self.assertIn("- Fix crash in reconfigure ([#12](u12), CHEF-2)", md)
        self.assertIn("| [CVE-2026-1](https://nvd.nist.gov/vuln/detail/CVE-2026-1) | High | rack | 3.2.5 → 3.2.6 "
                      "| [#13](u13) |", md)
        self.assertIn("| Rack | 3.2.5 | 3.2.6 | Security | [#13](u13) |", md)
        self.assertIn("- OpenSearch 1.3.20", md)
        self.assertNotIn("- Rack 3.2.6", md.split("### Service versions")[1])
        self.assertEqual(review["unclassified"], ["#14 Mystery"])
        self.assertIn("None in this release.", md)

    def test_open_vulnerabilities_only_in_review(self):
        cfg = relnotes.load_product("chef-server")
        diff = {"fixed": [], "introduced": [{"id": "CVE-2026-9", "severity": "Critical", "package": "x",
                                             "version": "1"}], "still_present": []}
        md, review = render.render(cfg, {"from": "1", "to": "2", "prs": []}, None, None, [diff], [], {}, "d")
        self.assertNotIn("CVE-2026-9", md)
        self.assertIn("Introduced since previous release: 1", render.render_review(review, [diff]))


if __name__ == "__main__":
    unittest.main()
