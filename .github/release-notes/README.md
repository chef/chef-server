# Release notes and security automation

This folder holds the shared library and per-product configuration for these Copilot agents:

- `.github/agents/release-notes.agent.md`
- `.github/agents/security.agent.md`

and these skills:

- `.github/skills/release-notes`
- `.github/skills/release-changes`
- `.github/skills/component-inventory`
- `.github/skills/grype-scan`
- `.github/skills/sbom`

```
.github/
  agents/
    release-notes.agent.md     # drafts customer-facing notes
    security.agent.md          # grype + SBOM evidence
  skills/
    release-notes/             # renderer + end-to-end release_notes.sh
    release-changes/           # PRs/Jira from CHANGELOG or git, classified
    component-inventory/       # major 3rd-party component versions + diff
    grype-scan/                # grype scan, normalize, summarize, diff
    sbom/                      # GitHub / Black Duck / syft SBOM collection
  release-notes/
    lib/relnotes.py            # shared helpers (stdlib only)
    products/<product>.json    # everything product-specific
    products/_template.json
    tests/                     # python3 -m unittest discover -s .github/release-notes/tests
dev-docs/
  security/grype/<version>/    # normalized scans + diffs (written by grype-scan)
  security/sbom/<version>/     # SBOMs (written by sbom)
  release-notes/<version>.md   # drafts (written by release-notes)
```

The scripts need only Python 3.8+ and bash. `gh`, `grype`, and `syft` are used when they're available.

## How data flows

1. `release-changes`: CHANGELOG.md or git, plus GitHub labels and files, plus Jira issue types, produce classified PRs.
2. `component-inventory`: version manifest and lockfiles at the old and new refs produce the Service versions and Updated components sections.
3. `sbom` and `grype-scan` (security agent): scans of both releases with matching names produce a list of fixed CVEs.
4. `release-notes`: combines the above with agent-written `notes.json` into the fixed layout, plus an internal review report.

## Adding another product (Chef Automate, Chef Infra Client, Chef 360, …)

Everything product-specific lives in `products/<product>.json`. The agents, skills, and scripts don't change.

1. Copy `products/_template.json` to `products/<product>.json`.
2. Fill in:
   - `github_repo`, `version_file`, `tag_format`
   - `changelog`: use the `expeditor` format if the repo has an Expeditor `CHANGELOG.md`; otherwise use `--source git`.
   - `docs.release_notes_path`
3. Set `security.scan_targets` to the targets you can scan for that product: the installed package directory, container images (`docker:…`), or SBOMs.
4. List the major components and their tiers. A component is major if it's:
   - a separately running bundled service (tier A),
   - a language runtime or crypto library (tier B), or
   - a framework or library that handles untrusted input, authentication, or persistence (tiers C/D).
5. Tune `classification` to the repo's labels and the paths that don't ship.
6. To use the agents from another repository, copy the folders listed under "Files to copy" below into it. Alternatively, keep them in an org-level shared repository so all products use one copy.

### Starting points for other products

These are suggestions to confirm against each repo:

| Product | Version sources | Likely tier A/B components | Scan targets |
|---|---|---|---|
| Chef Automate | Habitat package manifest (`manifest.json` published per build), `go.mod`, `components/*/habitat/plan.sh` | OpenSearch/Elasticsearch, PostgreSQL, Habitat supervisor, nginx/automate-load-balancer, Go toolchain | `docker:`/hab package exports, published SBOM |
| Chef Infra Client | Omnibus `/opt/chef/version-manifest.json`, `omnibus_overrides.rb`, `Gemfile.lock` | Ruby, OpenSSL, libxml2/libxslt (Nokogiri), Ohai, Train | `dir:/opt/chef`, Windows MSI SBOM |
| Chef 360 | Helm chart / image tags, `go.mod`, `package-lock.json` | PostgreSQL, message broker, API gateway, Go/Node runtimes | `docker:` images per service, Black Duck SBOM |

## Files to copy into another repository

- `.github/agents/release-notes.agent.md`, `.github/agents/security.agent.md`
- `.github/skills/{release-notes,release-changes,component-inventory,grype-scan,sbom}/`
- `.github/release-notes/lib/`, `.github/release-notes/products/_template.json`, `.github/release-notes/tests/`
- `dev-docs/security/README.md`

## Tests

```bash
python3 -m unittest discover -s .github/release-notes/tests -v
```
