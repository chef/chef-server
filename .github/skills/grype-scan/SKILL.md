---
name: grype-scan
description: Scan a build, SBOM, container image, or the repository lockfiles for known vulnerabilities with grype, store normalized results in dev-docs/security/grype/<version>/, and diff two releases to show which CVEs were fixed, introduced, or are still present. Use this when asked to scan for CVEs or vulnerabilities, check whether a CVE is fixed, or prepare the security section of release notes.
---

# grype-scan

Scans for vulnerabilities and saves the results in a fixed folder layout, so any two releases can be compared.

## Output layout

```
dev-docs/security/grype/<version>/
  <name>.json                      # normalized findings (commit this)
  <name>.md                        # readable summary of one scan
  diff-<name>-from-<prev>.json     # fixed / introduced / still_present
  diff-<name>-from-<prev>.md
```

`<name>` identifies the scan target, for example `omnibus`, `source`, or `sbom-github`. Use the **same name for every release**, because diffs only compare files that have the same name.

Don't commit `*.raw.json` files. They're large and the normalized file has everything the diff needs.

## Scan targets

The available targets for each product are listed in `.github/release-notes/products/<product>.json` under `security.scan_targets`. For chef-server:

| Name | Target | Notes |
|---|---|---|
| `omnibus` | `dir:/opt/opscode` | Most complete view of what ships. Run on a host or container where the `.deb`/`.rpm` is installed. |
| `source` | `dir:.` | Ruby and Erlang lockfiles only. Works without a build, but misses bundled C/Java services (OpenSearch, PostgreSQL, OpenResty). |
| `sbom-github` / `sbom-blackduck` | `sbom:dev-docs/security/sbom/<version>/<file>` | Scans an SBOM collected by the `sbom` skill. |

## Steps

1. **Pick the version.** It defaults to the `VERSION` file. Pass `--version` when scanning an older release.
2. **Run the scan.** grype must be installed; `--install` installs it into `~/.local/bin`.
   ```bash
   .github/skills/grype-scan/scripts/grype_scan.sh --name omnibus dir:/opt/opscode
   .github/skills/grype-scan/scripts/grype_scan.sh --name source dir:.
   .github/skills/grype-scan/scripts/grype_scan.sh --name sbom-github \
       sbom:dev-docs/security/sbom/15.10.127/github.spdx.json
   ```
3. **Make sure the previous release has the same scan.** If `dev-docs/security/grype/<prev>/<name>.json` is missing:
   - Install the previous package and scan it with `--version <prev>`, or
   - Scan the previous release's SBOM.

   Only then diff:
   ```bash
   python3 .github/skills/grype-scan/scripts/grype_report.py diff \
     dev-docs/security/grype/15.10.125/omnibus.json dev-docs/security/grype/15.10.127/omnibus.json \
     -o dev-docs/security/grype/15.10.127/diff-omnibus-from-15.10.125.json \
     --markdown dev-docs/security/grype/15.10.127/diff-omnibus-from-15.10.125.md
   ```
   The `release-notes` orchestrator runs this diff automatically for every scan name present in both version folders.
4. **Report the results.** Give counts by severity, then list fixed CVEs and any newly introduced Critical/High findings.

## Rules

- **Fixed** means the vulnerability was in the old scan and is gone from the new scan of the **same target type**. Never compare an `omnibus` scan with a `source` scan.
- grype can't see inside some vendor builds, for example TuxCare OpenSearch and PostgreSQL, so their fixes don't show up as fixed. Record those as **vendor-asserted** in a `cve-extra.json` file for the `release-notes` skill. Every entry needs a citation: a Jira issue, a PR, or a vendor advisory.
- Never put **introduced** or **still present** vulnerabilities in customer-facing release notes. They belong in the internal review report only.
- `dev-docs/` is public in open-source repositories. Results are committed only after a release is published, and only as normalized files. If the release isn't public yet, don't commit them; ask the user first.
- Normalization keys each finding on its CVE ID (falling back to the GHSA ID), so a GHSA and its CVE are not counted twice.
