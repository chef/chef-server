---
name: release-notes
description: Draft customer-facing release notes for a product build (Chef Infra Server by default; also works for other products with a config in .github/release-notes/products/). Combines PRs and Jira issues, grype CVE diffs, and the component inventory into a fixed, easy-to-scan layout, plus an internal review report. Use this when asked to write, generate, or check release notes.
---

# release-notes

The goal is **readability**: a customer should be able to check in seconds whether *their* CVE, bug, or feature request is in this release.

## Fixed layout

Every section appears every time. Empty sections say "None in this release."

1. `## <Product> <version>`, then the release date, a 1–3 sentence highlights paragraph, and "since <previous version>".
2. `### Upgrade notes and breaking changes`
3. `### New features`
4. `### Improvements`
5. `### Bug fixes`
6. `### Security fixes`: one CVE table (CVE linked to NVD | Severity | Component | Version change | Reference), sorted by component, then severity, then CVE ID. After the table, list other hardening items that have no CVE.
7. `### Updated components`: a Previous → New table for major components whose version changed (Reason column is Security or Maintenance), followed by any dependency PRs that didn't match a row.
8. `### Service versions`: the full snapshot of tier A (bundled services) and tier B (runtimes).
9. `### Known issues`: only when there are any.

## Inputs and their sources

| Section | Source |
|---|---|
| Features, improvements, bug fixes, upgrade notes | `release-changes` skill (Expeditor CHANGELOG + GitHub PRs + Jira issue types) |
| Security fixes | `grype-scan` skill diffs (`dev-docs/security/grype/<ver>/diff-*.json`) plus `--cve-extra` for vendor-asserted fixes (TuxCare, etc.), matched to PRs |
| Updated components, Service versions | `component-inventory` skill (version manifest, lockfiles, overrides) |
| SBOM evidence | `sbom` skill (`dev-docs/security/sbom/<ver>/`) |

## Steps

1. **Identify the product, this version, and the previous published version.** Use `--product` (default `chef-server`).
2. **Make sure security data exists.** Check that `dev-docs/security/grype/<prev>/` and `dev-docs/security/grype/<this>/` contain scans with matching names. If they don't, delegate to the **security** agent, or run the `grype-scan` skill yourself.
3. **Run the pipeline:**
   ```bash
   .github/skills/release-notes/scripts/release_notes.sh --product chef-server --from 15.10.125 \
     --github [--jira /tmp/jira.json] [--new-manifest /opt/opscode/version-manifest.json] \
     [--old-manifest prev-version-manifest.json] [--cve-extra cve-extra.json] [--notes notes.json]
   ```
   Outputs:
   - Draft: `dev-docs/release-notes/<version>.md`
   - Internal review report: `$TMPDIR/release-notes-<product>-<version>/review.md`
4. **Rewrite for customers.** Write `notes.json` and re-run step 3. Each PR bullet becomes one sentence written from the customer's point of view: past tense ("Fixed…", "Added…", "Updated…"), naming the symptom or capability, with commands and config keys in backticks. Never include branch names or people's names.
   ```json
   {
     "highlights": "This release updates Rack and Valkey to resolve 3 CVEs and fixes log rotation in Bifrost.",
     "upgrade_notes": ["Run `chef-server-ctl reconfigure` after upgrading to remove stale gem versions."],
     "hardening": ["The `/version` endpoint no longer exposes internal library versions."],
     "known_issues": [],
     "prs": {
       "4239": {"text": "Updated faraday to 2.14.3 in `chef-server-ctl` and `oc-id`.", "section": "security"},
       "4236": {"text": "Updated OpenResty to 1.31.1.1 in Habitat packages."}
     }
   }
   ```
   `cve-extra.json` lists vendor-asserted fixes. Every entry needs a citation:
   ```json
   [{"id": "CVE-2025-49844", "component": "Valkey", "severity": "Critical",
     "previous": "7.2.10", "fixed_in": "7.2.11", "reference": "[CHEF-12345](...)"}]
   ```
5. **Work through the review report** until every list is empty or explained:
   - unclassified PRs
   - dependency PRs with no matching component
   - CVEs with no matching PR
   - missing versions
   - consistency warnings, such as a CVE fixed in a component whose version did not change
6. **Present the draft** and summarize anything still open. Don't publish it or open a docs PR unless asked. When asked, the target is `docs.release_notes_repo` / `docs.release_notes_path` from the product config; insert the draft at the top of that file.

## Style rules

- Each CVE ID appears once, in the table, linked. Don't also list it in prose.
- Always show both versions: "Updated Rack from 3.2.5 to 3.2.6".
- End every bullet with its references: `([#1234](…), CHEF-5678)`.
- Use the same component names in every release, taken from the product config. Write "OpenSearch", never "Opensearch".
- Service versions must agree with the security table. The renderer warns when they don't.
- Never list introduced or unfixed vulnerabilities in customer-facing notes.
