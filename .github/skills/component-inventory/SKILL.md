---
name: component-inventory
description: List the versions of a product's major third-party components (bundled services such as OpenSearch, PostgreSQL, and OpenResty; runtimes such as Erlang/OTP, Ruby, and OpenSSL; key Erlang and Ruby frameworks) at any git ref or build, and diff two inventories. Use this to fill the "Service versions" and "Updated components" release-notes sections, or to answer "which version of X ships in release Y".
---

# component-inventory

Reads component versions from the sources listed in the product config, so release notes never contradict what actually ships.

## Which components count as major

Components are defined in `.github/release-notes/products/<product>.json` under `components`. Each one has a tier:

| Tier | Meaning | Release-notes treatment |
|---|---|---|
| A | Bundled services the customer runs or sizes (OpenResty, PostgreSQL, OpenSearch, OpenJRE, Valkey, HAProxy) | Always listed under **Service versions** |
| B | Language runtimes and platform libraries (Erlang/OTP, Ruby, OpenSSL/FIPS, Node.js, Sqitch, Perl) | Always listed under **Service versions** |
| C | Erlang frameworks/libraries that handle untrusted input, authentication, or data (webmachine, mochiweb, epgsql, sqerl, pooler, ibrowse, jiffy, erlcloud, chef_authn, bcrypt) | Shown under **Updated components** only when the version changes |
| D | Ruby frameworks and gems (Rails, Rack, Doorkeeper, Nokogiri, chef, knife, plus gems with a history of CVEs) | Shown under **Updated components** only when the version changes |

To add a component, add an entry to the config. Don't change the script.

## Version sources

The sources for a component are tried in order; the first one that returns a version wins.

| `type` | Reads |
|---|---|
| `version_manifest` | Omnibus `version-manifest.json` (`/opt/opscode/version-manifest.json` in an installed package). This is the authoritative source for tiers A and B. |
| `regex` | Any file in the repo, e.g. `omnibus_overrides.rb`, `habitat/plan.sh`, or `rebar.config` (`require_otp_vsn`). |
| `gemfile_lock` | Top-level specs in one or more `Gemfile.lock` files. If the files disagree, every version is shown (`7.3.2 / 7.3.3`). |
| `rebar_lock` | hex package version, git tag, branch, or a short commit SHA from `rebar.lock`. |

## Steps

1. **Inventory this build.** Pass the version manifest when you have it:
   ```bash
   python3 .github/skills/component-inventory/scripts/component_inventory.py --product chef-server \
     --version-manifest /opt/opscode/version-manifest.json -o /tmp/inv-new.json
   ```
2. **Inventory the previous release.** Read files at its tag; the tag must be fetched first (`git fetch --tags --unshallow`).
   ```bash
   python3 .github/skills/component-inventory/scripts/component_inventory.py --ref 15.10.125 \
     --version-manifest /tmp/15.10.125-version-manifest.json -o /tmp/inv-old.json
   ```
3. **Fill gaps from vendors.** Use `--set "OpenSearch=1.3.20-tuxcare-1.0.3"` for vendor builds that aren't in any manifest. Cite where each value came from.
4. **Diff:**
   ```bash
   python3 .github/skills/component-inventory/scripts/component_inventory.py --diff /tmp/inv-old.json /tmp/inv-new.json
   ```

## Rules

- Never guess a version. If a tier A or B component has no version, say so and ask for the version manifest of the build.
- Without a version manifest, source pins (for example `omnibus_overrides.rb`) may be commented out or out of date. Treat versions from source pins as provisional.
