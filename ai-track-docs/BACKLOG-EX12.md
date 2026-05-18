# Backlog — Crawl Track Findings

**Exercise**: 12 — Crawl Track v2.3.1  
**Generated from**: Exercises 0–11 repo analysis  
**Date**: 2026-05-18  
**Module under study**: `src/bookshelf/` (primary), `omnibus_overrides.rb`, `.github/`  

Items are ordered by risk × fix-effort. Each includes a code link, acceptance
criteria, and a suggested label.

---

## BACKLOG-001 · Fix 4 un-pinned branch entries in `rebar.lock`

**Label**: `tech-debt` `reliability` `bookshelf`  
**Size**: Medium  
**Priority**: High

### Problem

Four dependencies in `src/bookshelf/rebar.lock` record `{branch, "..."}` instead
of `{ref, "sha"}`. This means a `./rebar3 upgrade` or a clean checkout that skips
the lock file will silently pull the tip of those branches — potentially introducing
breaking changes with no code review.

```
# src/bookshelf/rebar.lock
{<<"erlcloud">>,  {git, ..., {branch, "CHEF-11677/CHEF-12498/lbaker"}}, 0}
{<<"mini_s3">>,   {git, ..., {branch, "CHEF-11677/CHEF-12498/lbaker"}}, 0}
{<<"erlsom">>,    {git, ..., {branch, "integer_long_string_probs2"}},   0}
{<<"sqerl">>,     {git, ..., {branch, "shahid/sqerl-erl27.3-pg16.1"}}, 0}
```

Three of the four are on personal developer branches (`lbaker/`, `shahid/`).

### Acceptance Criteria

- [ ] Each of the 4 deps in `rebar.config` is pinned to a `{ref, "sha"}` or a
  stable `{tag, "..."}` in the declared source
- [ ] `rebar.lock` contains no `{branch, ...}` entries for these four deps
- [ ] The targeted SHAs are the same commits currently in use (no unintended upgrades)
- [ ] `./rebar3 eunit` passes after the change

### Code Links

- `src/bookshelf/rebar.config` lines 19–41 (dep declarations)
- `src/bookshelf/rebar.lock` lines 29–32, 49–52, 69–72, 101–104
- Reference: `ai-track-docs/dependencies.md` §"Branch-tracked in lock file"

---

## BACKLOG-002 · Enable Erlang unit tests in GitHub Actions CI

**Label**: `ci` `testing` `reliability`  
**Size**: Small–Medium  
**Priority**: High

### Problem

The GitHub Actions workflow stub has `build: false` and `unit-tests: false`.
Erlang unit tests, dialyzer, elvis style checks, and Ruby RSpec are **not
executed by CI** — they only run if a developer remembers to run them locally.
A bad merge will not be caught until the build is attempted manually.

```yaml
# .github/workflows/ci-main-pull-request-stub.yml lines 96–99
build: false
unit-tests: false
```

### Acceptance Criteria

- [ ] Erlang unit tests (`rebar3 eunit`) run on every PR to `main`
- [ ] Dialyzer runs on every PR (or is explicitly deferred with a documented
  reason)
- [ ] Elvis style check runs on every PR
- [ ] A failed unit test causes the CI check to fail and blocks merge
- [ ] `scripts/test-bookshelf-unit.sh` (or equivalent) is usable as the CI
  entry point if the GCC/jiffy issue is present on CI runners

### Code Links

- `.github/workflows/ci-main-pull-request-stub.yml` lines 96–99
- `scripts/test-bookshelf-unit.sh` (local runner created in Ex10)
- `src/bookshelf/Makefile` target `ci:` (line 67) — full CI sequence including CT

---

## BACKLOG-003 · Propagate `req_id` to all remaining untraced error logs in bookshelf

**Label**: `observability` `bookshelf` `logging`  
**Size**: Small  
**Priority**: Medium

### Problem

Exercise 9 fixed 3 high-value error log lines. However 14 additional
`error_logger:error_msg` calls in `bksw_wm_sql_object.erl`,
`bksw_wm_object.erl`, and `bksw_io.erl` still emit without a `req_id`
trace key. During an incident, operators cannot correlate these error messages
to a specific client request.

```erlang
%% bksw_wm_sql_object.erl:229 — no trace context
error_logger:error_msg("Error occurred during content download: missing chunk ~p ~p ~n",
                       [ChunkId, DbFile])

%% bksw_wm_object.erl:187 — no trace context
error_logger:error_msg("Error occurred during content download: ~p~n", [Error])
```

### Acceptance Criteria

- [ ] All `error_logger:error_msg` / `lager:error` calls in request-handling
  modules (`bksw_wm_sql_object.erl`, `bksw_wm_object.erl`) include
  `req_id=~s` as the first field when `#context{reqid}` is in scope
- [ ] Log lines use `key=value` token format (grep-friendly)
- [ ] `bksw_io.erl` filesystem errors are assessed — req_id is NOT in scope
  there (correct), but bucket/path context should be present (already is)
- [ ] No existing tests broken

### Code Links

- `src/bookshelf/src/bksw_wm_sql_object.erl` lines 174, 178, 199, 229, 232,
  240, 245, 254, 271, 282
- `src/bookshelf/src/bksw_wm_object.erl` lines 163, 187, 198
- Reference: `ai-track-docs/logging.md` §"The req_id Convention"

---

## BACKLOG-004 · Resolve OTP version mismatch between `rebar.config` and `omnibus_overrides.rb`

**Label**: `build` `reliability` `tech-debt`  
**Size**: Small  
**Priority**: Medium

### Problem

All three Erlang services (`oc_erchef`, `oc_bifrost`, `bookshelf`) require
OTP `26.2.5.15` in `rebar.config`. The Omnibus build override for Erlang is
commented out but specifies `26.2.5.14` — one patch behind:

```erlang
%% src/bookshelf/rebar.config:5  (same in oc_erchef and oc_bifrost)
{require_otp_vsn, "26.2.5.15"}.
```

```ruby
# omnibus_overrides.rb:4
#override :erlang, version: "26.2.5.14"
```

If the Erlang override is uncommented without updating the version, all three
services will fail `rebar3` OTP version enforcement during an omnibus build.
The mismatch is a latent build-break trap.

### Acceptance Criteria

- [ ] The commented-out `:erlang` override in `omnibus_overrides.rb` is either
  removed or updated to `26.2.5.15`
- [ ] A comment is added explaining what OTP version the services require and
  where to verify it
- [ ] `doc/FrequentTasks.md` checklist (referenced in `omnibus_overrides.rb`)
  is updated if it lists the Erlang version

### Code Links

- `omnibus_overrides.rb` line 4
- `src/bookshelf/rebar.config` line 5
- `src/oc_erchef/rebar.config` line 5
- `src/oc_bifrost/rebar.config` line 12

---

## BACKLOG-005 · Add `trufflehog` / `gitleaks` allowlist for test fixture key material

**Label**: `security` `ci` `dx`  
**Size**: Small  
**Priority**: Medium

### Problem

The GitHub Actions CI runs `perform-trufflehog-scan: true` with
`fail-trufflehog-on-secrets-found: true`. The repository contains 5 committed
`.pem` files that are intentional test fixtures and Habitat config templates
(not real credentials). Without an allowlist, these will trigger scan failures
on every CI run or prevent onboarding of the scanner.

Exercise 8 added a plain-text header to `spec/fixtures/pivotal.pem` to make
intent clear, but no machine-readable allowlist file exists.

```
src/chef-server-ctl/spec/fixtures/pivotal.pem   ← RSA key (test fixture)
src/chef-server-ctl/habitat/config/pivotal.pem  ← Habitat template
src/chef-server-ctl/habitat/config/webui_priv.pem ← Habitat template
src/oc_erchef/apps/chef_objects/test/cert.pem   ← test certificate
src/oc_erchef/apps/oc_chef_wm/itest/public.pem  ← test public key
```

Additionally, `zuperzecret` in `setup_helper.erl` will trip naive entropy-based
scanners.

### Acceptance Criteria

- [ ] A `.trufflehog.yml` or `.gitleaks.toml` allowlist file exists at repo root
  listing the 5 `.pem` files by path (or by file hash) as known-safe
- [ ] The `zuperzecret` literal in `itest/setup_helper.erl` is covered by an
  inline suppression comment (`# trufflehog:ignore` or equivalent)
- [ ] CI secret scan passes with the allowlist in place
- [ ] The allowlist file includes a comment explaining why each entry is excluded

### Code Links

- `src/chef-server-ctl/spec/fixtures/pivotal.pem` (header added in Ex8)
- `src/oc_erchef/apps/oc_chef_wm/itest/setup_helper.erl` line 218
- `.github/workflows/ci-main-pull-request-stub.yml` lines 71–73
- Reference: `ai-track-docs/security-hygiene.md` §"Test fixture private key"

---

## Summary Table

| ID | Title | Priority | Size | Label |
|----|-------|----------|------|-------|
| BACKLOG-001 | Pin 4 branch-tracked `rebar.lock` deps to SHAs | 🔴 High | M | `tech-debt` `reliability` |
| BACKLOG-002 | Enable unit tests in GitHub Actions CI | 🔴 High | S–M | `ci` `testing` |
| BACKLOG-003 | Propagate `req_id` to remaining 14 error logs | 🟡 Medium | S | `observability` |
| BACKLOG-004 | Fix OTP version mismatch in omnibus override | 🟡 Medium | S | `build` `reliability` |
| BACKLOG-005 | Add trufflehog/gitleaks allowlist for test PEMs | 🟡 Medium | S | `security` `ci` |
