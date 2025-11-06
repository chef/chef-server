# oc-chef-pedant Testing Guide

This guide standardizes how to run the oc-chef-pedant suite across environments (chef-server vs chef-zero) and documents recent helper scope changes, risk notes, and the validation matrix post ActiveSupport removal.

---

## 1. Supported Execution Modes

| Mode | Target | Purpose | Invocation Summary |
|------|--------|---------|--------------------|
| Server (Embedded) | Full Chef Server (dev VM / omnibus install) | Primary regression signal | `chef-server-ctl test` (inside VM) or `bin/oc-chef-pedant -c pedant_config.rb` |
| chef-zero (Released Gem) | Upstream chef-zero gem matching Chef 18 baseline | Fast API contract smoke, isolate server-side regressions | Rake task / direct script (see below) |
| chef-zero (Branch Override) | Branch removing legacy Chef::VERSION references | Validate removal of ActiveSupport & version coupling | Export `CHEF_VERSION` or Bundler override Gemfile |

> Ruby 3.1.7 is the reference version (Chef 18 parity). Avoid 3.4+ until broader compatibility is validated.

---

## 2. Quick Commands

### 2.1 Inside Dev VM (full server)

```bash
chef-server-ctl test --all            # Full pedant + internal org tests
chef-server-ctl test                  # Default subset
chef-server-ctl test --all --exclude-internal-orgs
```

### 2.2 Local (without full server) using chef-zero

From `oc-chef-pedant/`:

```bash
# Ensure bundle resolves to the released chef-zero
bundle install
rake chef_zero_spec
```

### 2.3 Using Branch Override of chef-zero

Use a dedicated Gemfile (example: `scripts/bk_tests/chef_zero-Gemfile`) or add a block like:

```ruby
# Gemfile.local snippet
# gem 'chef-zero', git: 'https://github.com/chef/chef-zero', branch: 'remove-chef-version'
```

Then:

```bash
BUNDLE_GEMFILE=scripts/bk_tests/chef_zero-Gemfile bundle install
BUNDLE_GEMFILE=scripts/bk_tests/chef_zero-Gemfile rake chef_zero_spec
```

### 2.4 Focused Spec Samples

```bash
bundle exec rspec spec/api/keys/user_keys_spec.rb:930-970
bundle exec rspec spec/api/server_api_version_spec.rb
```

---

## 3. Helper Scope Changes (IMPORTANT)

Removed class-level singleton helpers:

- `platform`
- `api_url`

Rationale: They caused large clusters of WrongScope failures when indirectly invoked in `before(:all)` and polluted example group scope. Use patterns below:

| Context | Old (removed) | New (explicit) |
|---------|---------------|----------------|
| before(:all)/after(:all) | `platform.create_user(name)` | `Pedant::Config.pedant_platform.create_user(name)` |
| Example / let / before(:each) | `platform` (still OK via instance helper) | `platform` |
| Building URLs (group scope) | `api_url("/nodes")` | `Pedant::Config.pedant_platform.api_url("/nodes")` |

Instance helpers remain: `platform`, `api_url(path)` inside example scope only.

---

## 4. Validation Matrix

| Dimension | Set | Notes |
|-----------|-----|-------|
| Ruby | 3.1.7 | Canonical; matches Chef 18 runtime |
| Platform Mode | Full Server, chef-zero (released), chef-zero (branch) | Branch mode only when validating coupling removal |
| Spec Slice | Smoke (server_api_version_spec + a CRUD resource), Core (default), Full (`--all`) | Smoke runs should complete < 2 min on typical dev hardware |
| Headers | `X-Chef-Version` hardcoded KNIFE_VERSION | No dynamic Chef::VERSION dependency |
| API Versions | v0 (legacy), negotiation path (`/server_api_version`) | Ensure `server_api_version_spec` passes in all modes |

Minimum CI gates after recent changes:

1. Server Mode Core suite: PASS
2. chef-zero Released Smoke suite: PASS (server_api_version + basic ACL or keys spec subset)
3. Branch Override (only while feature branch active): Smoke suite PASS
4. Full Server `--all` (nightly) PASS

---

## 5. Risk & Compatibility Notes

| Area | Risk | Mitigation |
|------|------|------------|
| Helper Scope Removal | Hidden reliance on class-level `platform` in `before(:all)` blocks causing NameError later | Grep for `before(:all)` + `platform.`; replace with explicit config reference |
| ActiveSupport Removal | Missed implicit inflector / Hash key conversions | Added targeted replacements (`pedant/concern`, `stringify_keys` shim) & core suite green before broader matrix |
| Version Decoupling | Tools expecting dynamic Chef::VERSION in headers | Hardcoded `KNIFE_VERSION` unchanged; document expectation |
| chef-zero Branch Divergence | Drift from released gem causing false positives | Keep branch usage confined to feature PR validation; do not gate merges on branch-only passes |
| Ruby Version Skew | Newer Ruby 3.3/3.4 differences (warning categories, RSpec timing) | Pin CI to 3.1.7; add optional future lane once core stabilized |

Rollback Strategy: Re-introducing class-level helpers is low complexity but discouraged; prefer updating any lingering specs. ActiveSupport dependency should not be restored—issues should be fixed by adding narrowly scoped utilities.

---

## 6. Troubleshooting Quick Table

| Symptom | Likely Cause | Action |
|---------|--------------|--------|
| 1000+ WrongScope / method missing `platform` failures | A `before(:all)` still calling instance helper | Replace with `Pedant::Config.pedant_platform` |
| 401/403 explosion under chef-zero only | Org/user provisioning order or missing association | Re-run single spec with `--backtrace`, inspect platform creation logs |
| All requests 500 in chef-zero mode | Wrong chef-zero branch / dependency mismatch | Confirm Gemfile lock; run `ruby -v`; re-bundle with released gem |
| Version negotiation failures | Header mismatch / test expecting dynamic version | Inspect `X-Chef-Version` in captured request log |

---

## 7. Adding New Tests – Best Practices

- Use `let` for per-example dynamic data; use `shared(:symbol)` only when a value must be accessible inside `before(:all)`.
- Avoid creating real users/orgs in `before(:all)` unless they are immutable and shared; prefer `before(:each)` for mutable objects.
- Always clean up with explicit `Pedant::Config.pedant_platform.delete_*` calls in `after(:all)` for created actors.
- For new endpoints: add smoke coverage to the Smoke slice (small curated set) to keep early failures tight.

---

## 8. Future Enhancements (Optional Backlog)

- Introduce a tagged `:smoke` subset with < 50 examples for <60s validation.
- Add script to auto-rewrite lingering `platform.` calls in group hooks.
- Parameterize KNIFE_VERSION via env var for experimental clients.

---

## 9. At-a-Glance Checklist (PR Author)

- [ ] Core suite passes locally (server mode)
- [ ] Smoke suite passes with released chef-zero
- [ ] No new `before(:all)` using instance helpers
- [ ] Added/updated docs if new config flags introduced

---
Last updated: 2025-11-06
