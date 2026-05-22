# Dependency Audit & Pinning Policy

**Exercise**: 7 — Crawl Track v2.3.1  
**Scope**: `src/bookshelf/` (Erlang) + platform overrides (`omnibus_overrides.rb`)  
**Date**: 2026-05-18  
**Files examined**: `src/bookshelf/rebar.config`, `src/bookshelf/rebar.lock`, `omnibus_overrides.rb`

---

## How rebar3 pinning works

| Source | Role | Risk if mutable |
|--------|------|-----------------|
| `rebar.config` — `{branch, "..."}` | Declared intention | HIGH — resolves to tip-of-branch at fetch time |
| `rebar.config` — `{tag, "..."}` | Semantic version label | LOW — tags *should* be immutable; can still be deleted |
| `rebar.config` — `{ref, "sha"}` | Exact commit SHA | NONE — immutable |
| `rebar.lock` — `{ref, "sha"}` | What was actually fetched | NONE — but only used if `rebar.lock` is present and not skipped |
| `rebar.lock` — `{branch, "..."}` | **Not frozen** | HIGH — `rebar3 upgrade` or a missing lock will re-resolve |

The lock file wins over `rebar.config` for reproducible builds **only if** it contains
`{ref, sha}` entries. When the lock file itself records a `{branch, ...}`, the next
`rebar3 upgrade` or clean checkout can pull a different commit silently.

---

## Bookshelf Erlang Dependencies

### 🔴 Branch-tracked in lock file (highest risk)

These four entries appear in `rebar.lock` with `{branch, ...}` instead of `{ref, sha}`.
They are **not reproducibly pinned** and can drift on any clean fetch.

| Dep | Branch in lock | Owner | Risk note |
|-----|---------------|-------|-----------|
| `erlcloud` | `CHEF-11677/CHEF-12498/lbaker` | chef fork | Personal feature branch; not stable |
| `mini_s3` | `CHEF-11677/CHEF-12498/lbaker` | chef fork | Same branch as erlcloud — coupled |
| `erlsom` | `integer_long_string_probs2` | chef fork | Feature branch; unclear if merged upstream |
| `sqerl` | `shahid/sqerl-erl27.3-pg16.1` | chef fork | OTP 27 / PG 16.1 compatibility branch |

**Recommended action** (do not execute without team review):  
Run `./rebar3 lock` after a clean fetch to resolve to SHAs, then pin those SHAs
in `rebar.config` as `{ref, "sha"}`. Treat the resulting lock file as the source
of truth and commit it.

### 🟡 Branch-tracked in config, ref-frozen in lock (medium risk)

These are declared as `{branch, ...}` in `rebar.config` but the lock file has resolved
them to a specific `{ref, sha}`. They are reproducible **today** but will drift on the
next intentional `rebar3 upgrade`.

| Dep | Config branch | Lock ref (first 8) | Notes |
|-----|--------------|-------------------|-------|
| `lager` | `master` | `a140ea93` | Core logging framework |
| `cf` | `master` | `2bcf0040` | Color formatting |
| `chef_secrets` | `main` | `a690fb9f` | Secret management |
| `envy` | `master` | `0148fb4b` | Env var helpers |
| `erlware_commons` | `lbaker/fix_for_ftmap` | `f511ed87` | Personal fix branch |
| `mixer` | `master` | `d5f58392` | Module function injection |
| `mochiweb` | `main` | `666ac57d` | HTTP server |
| `observer_cli` | `master` | `baa70569` | Runtime introspection |
| `opscoderl_wm` | `main` | `6495dd0f` | Webmachine wrapper |
| `sync` | `master` | `7c9367e7` | Dev hot-reload (test only) |

**Recommended action**: No immediate change needed. When any of these are intentionally
upgraded, update the lock file and commit the new SHA pair together.

### 🟢 Well-pinned (low risk)

| Dep | Pin type | Value | Notes |
|-----|----------|-------|-------|
| `iso8601` | `{tag, "1.2.3"}` | `4603fc81` in lock | Only tag-pinned dep; lock also has SHA |
| `meck` | `{ref, "5aaa24886..."}` | same in lock | Explicitly pinned with comment explaining why |

---

## OTP Version Constraint

`rebar.config` line 5:
```erlang
{require_otp_vsn, "26.2.5.15"}.
```

`omnibus_overrides.rb` (commented out):
```ruby
#override :erlang, version: "26.2.5.14"
```

⚠️ **Mismatch**: the config requires `.15` but the omnibus override (when re-enabled)
would install `.14`. If the Erlang override is ever uncommented without bumping the
version, builds will fail at the rebar3 OTP version check.

---

## Platform Dependencies (`omnibus_overrides.rb`)

| Package | Pinned version | Notes |
|---------|---------------|-------|
| Ruby | `3.1.7` | Ruby 3.1 EOL Apr 2025 — upgrade path needed (3.2 or 3.3) |
| OpenSSL | FIPS `3.1.2` | FIPS-enabled; version locked intentionally |
| Redis | `5.0.14` | Redis 5.x EOL Oct 2022 — significant upgrade risk |
| Perl | `5.34.0` | Perl 5.34 EOL; 5.38 current stable |
| runit | `2.1.1` | Pinned with known issue; `2.1.2` blocked by umbrella bug |
| sqitch | `0.973` | DB migration tool; version seems stable for current schema |
| logrotate | `3.19.0` | Utility; low risk |
| nokogiri | `1.18.9` | Ruby XML — critical security surface; keep updated |
| ohai | `v16.17.0` | Chef system profiler |
| omnibus-ctl | `main` | 🔴 Branch tracking — same risk as branch deps above |

---

## Pinning Policy (Recommended)

1. **Lock file is the contract.** The `rebar.lock` must be committed and reviewed on
   every intentional dependency change. Do not run `rebar3 upgrade` silently.

2. **No branch entries in the lock file.** Any `{branch, ...}` in `rebar.lock` is a
   build reproducibility bug. Convert by capturing the current SHA and setting
   `{ref, "sha"}` in `rebar.config`, then re-lock.

3. **Personal branches are temporary.** `erlcloud`/`mini_s3`/`erlsom`/`erlware_commons`
   all track personal branches (`lbaker/...`). These fixes should be either upstreamed
   to a stable branch/tag or captured as a Chef-internal fork tag.

4. **Platform upgrades are separate work.** Ruby 3.1 EOL and Redis 5.x EOL represent
   real maintenance debt but require coordinated omnibus work — do not address during
   Crawl track exercises.

5. **Do not upgrade during Crawl exercises.** The exercises are about observation and
   documentation, not upgrade execution. File findings as tech-debt tickets.

---

## What was NOT changed

No `rebar.config`, `rebar.lock`, or `omnibus_overrides.rb` files were modified.
This document is observation-only. Any pinning changes require a dedicated PR with
CI validation.
