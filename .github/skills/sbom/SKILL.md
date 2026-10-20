---
name: sbom
description: Collect a Software Bill of Materials (SBOM) for a release into dev-docs/security/sbom/<version>/. Exports the SBOM from GitHub's dependency graph, imports a Black Duck SBOM export (SPDX or CycloneDX), or generates one locally with syft. Use this when asked for an SBOM, or before running grype against an SBOM.
---

# sbom

Collects SBOMs into one folder per release so that grype can scan them and they can be compared across releases.

## Output layout

```
dev-docs/security/sbom/<version>/
  github.spdx.json             # GitHub dependency graph (SPDX 2.3)
  blackduck.spdx.json          # or blackduck.cyclonedx.json
  syft.spdx.json               # optional, generated locally from an installed package
```

## Sources

| Source | Command | Notes |
|---|---|---|
| GitHub | `.github/skills/sbom/scripts/sbom.sh github [--version V]` | Needs `gh` with repo read access. GitHub builds this SBOM from the **default branch's** manifests and lockfiles, so run it right after the release commit. It covers Gemfile.lock and other supported manifests. It does **not** cover rebar.lock or omnibus-bundled C/Java software. |
| Black Duck | `.github/skills/sbom/scripts/sbom.sh blackduck [--version V] FILE` | `FILE` is the SPDX/CycloneDX JSON (or a `.zip` containing it). Get it from the CI job `perform-blackduck-sca-scan` (project group `Chef-Infrastructure-Server`), or from the Black Duck UI: *Project version → Reports → Create SBOM*. This is the system of record for compliance. |
| syft | `.github/skills/sbom/scripts/sbom.sh syft [--version V] dir:/opt/opscode` | Most complete view of an installed omnibus package. Use it when Black Duck isn't available. |

## Steps

1. **Collect.** Use the sources available to you. Black Duck is preferred when you have its export; otherwise use GitHub and/or syft.
2. **Validate.** The script rejects anything that isn't SPDX or CycloneDX JSON.
3. **Scan (optional).** Pass the file to the `grype-scan` skill:
   `grype_scan.sh --name sbom-<source> sbom:dev-docs/security/sbom/<version>/<file>`.
   Use the same `--name` for every release so the scans can be diffed.
4. **Report** the file paths you wrote.

## Rules

- Never edit an SBOM by hand. Re-export it instead.
- SBOM files can be several MB. If one is over about 5 MB, ask the user before committing it, or compress it with `gzip` and say so.
