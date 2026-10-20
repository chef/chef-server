# Security evidence

This folder holds the vulnerability scan results and SBOMs for each release. The **security** Copilot agent (`.github/agents/security.agent.md`) writes them, and the **release-notes** agent reads them to build the "Security fixes" section.

```
dev-docs/security/
  grype/<version>/<name>.json                   normalized grype findings for one scan target
  grype/<version>/<name>.md                     readable summary
  grype/<version>/diff-<name>-from-<prev>.json  fixed / introduced / still present vs. previous release
  grype/<version>/diff-<name>-from-<prev>.md
  sbom/<version>/github.spdx.json               GitHub dependency-graph SBOM
  sbom/<version>/blackduck.<spdx|cyclonedx>.json  Black Duck SBOM export
  sbom/<version>/syft.spdx.json                 optional, generated from an installed package
```

Standard scan names for Chef Infra Server (see `.github/release-notes/products/chef-server.json`):

| Name | Target |
|---|---|
| `omnibus` | Installed package, `dir:/opt/opscode` |
| `source` | Repository lockfiles, `dir:.` |
| `sbom-github`, `sbom-blackduck` | The SBOMs above |

Diffs only compare scans with the same name, so use these names for every release.

## Commands

```bash
# SBOMs
.github/skills/sbom/scripts/sbom.sh github
.github/skills/sbom/scripts/sbom.sh blackduck ~/Downloads/chef-server-15.10.127-sbom.zip

# Scans (add --version for older releases, --install to fetch grype)
.github/skills/grype-scan/scripts/grype_scan.sh --name omnibus dir:/opt/opscode
.github/skills/grype-scan/scripts/grype_scan.sh --name sbom-github sbom:dev-docs/security/sbom/15.10.127/github.spdx.json

# Diff two releases
python3 .github/skills/grype-scan/scripts/grype_report.py diff \
  dev-docs/security/grype/15.10.125/omnibus.json dev-docs/security/grype/15.10.127/omnibus.json \
  --markdown dev-docs/security/grype/15.10.127/diff-omnibus-from-15.10.125.md \
  -o dev-docs/security/grype/15.10.127/diff-omnibus-from-15.10.125.json
```

## Policy

- This repository is public. Commit scan results only for releases that are already published, and never commit raw grype output (`*.raw.json`).
- Customer-facing release notes list **fixed** vulnerabilities only. Introduced or still-present findings are for internal triage.
- A fix that grype can't observe (for example vendor-patched TuxCare builds) is listed as vendor-asserted and needs a cited Jira issue, PR, or advisory.
