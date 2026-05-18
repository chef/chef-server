# Security & Secrets Hygiene Audit

**Exercise**: 8 — Crawl Track v2.3.1  
**Scope**: `src/` tree (Erlang + Ruby); platform config files  
**Date**: 2026-05-18  
**Method**: Pattern grep (`grep -rn`) across source, config, and fixture files; no dynamic scanning  
**Tools used**: ripgrep patterns for passwords, tokens, AWS key format, PEM headers, env patterns

---

## Summary

| Category | Finding | Severity | Action needed? |
|----------|---------|----------|---------------|
| Habitat PEM files | Template placeholders, not real keys | ✅ None | No |
| Test fixture private key | Real RSA key, test-only scope | 🟡 Low | Document intent |
| AWS key in test | AWS docs example key (`EXAMPLE`) | ✅ None | No |
| Bookshelf credential loading | `chef_secrets:get/2` at runtime | ✅ Good | No |
| Password redaction in audit log | `oc_chef_action` scrubs `"password"` field | ✅ Good | No |
| `.gitignore` missing secret patterns | No `.pem`/`.key`/`.env` exclusions | 🟡 Low | Consider adding |
| Test password literal | `<<"zuperzecret">>` in itest helper | 🟡 Low | Acceptable; note scope |
| `omnibus-ctl` on `main` branch | Supply-chain drift risk | 🟡 Low | See dep audit |
| No hardcoded production credentials | Confirmed across `src/` | ✅ Good | No |

---

## Detailed Findings

### ✅ Habitat PEM files are templates (not real keys)

```
src/chef-server-ctl/habitat/config/pivotal.pem   → {{cfg.secrets.chef-server.superuser_key}}
src/chef-server-ctl/habitat/config/webui_priv.pem → {{cfg.secrets.chef-server.webui_key}}
```

These files have `.pem` extensions but contain Habitat templating syntax only.
Actual key material is injected at runtime by the Habitat supervisor from the
secrets store. No credentials exposed.

### 🟡 Test fixture RSA private key

```
src/chef-server-ctl/spec/fixtures/pivotal.pem    — 27-line RSA PRIVATE KEY block
src/oc_erchef/apps/chef_objects/test/cert.pem    — test certificate
src/oc_erchef/apps/oc_chef_wm/itest/public.pem  — test public key
```

These are **test-only** keys used for RSA signing/verification unit tests.
They are not production credentials and are intentionally committed as test
infrastructure. However:

- No comments or README explains they are test-only — a future reader could
  flag them incorrectly in a secret scanner.
- They will trigger any `trufflehog`/`gitleaks`-style scanner without an
  explicit allow-list entry.

**Recommendation**: Add an `.allowlist` entry (or inline comment) if/when
a secret scanner is added to CI. Do not remove the keys — tests depend on them.

### ✅ AWS key in bookshelf tests is the AWS documentation example

```
src/bookshelf/test/bksw_sec_tests.erl:109
  Credential=AKIAIOSFODNN7EXAMPLE/...
```

`AKIAIOSFODNN7EXAMPLE` is the [canonical AWS documentation placeholder key](https://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html).
It is not a real access key and will not authenticate against any AWS account.
Used correctly here for S3 authorization header parsing tests.

### ✅ Bookshelf loads credentials via `chef_secrets` at runtime

```erlang
%% src/bookshelf/src/bksw_conf.erl lines 156–157
{ok, AWSAccessKey} = chef_secrets:get(<<"bookshelf">>, <<"access_key_id">>),
{ok, SecretKey}    = chef_secrets:get(<<"bookshelf">>, <<"secret_access_key">>),
```

Credentials are fetched from the `chef_secrets` service at startup, never
embedded in source or config files. This is the correct pattern for this
codebase. The `chef_secrets` library reads from an encrypted secrets file
managed by `chef-server-ctl`.

### ✅ Password field is redacted in audit/action logs

```erlang
%% src/oc_erchef/apps/oc_chef_wm/src/oc_chef_action.erl lines 100–102
case ej:get({"password"}, Msg) of
    undefined -> Msg;
    _ -> ej:set({<<"password">>}, Msg, ?REDACTED_PASSWORD)
end.
```

The `oc_chef_action` module explicitly scrubs the `"password"` field before
emitting action log events. This prevents password leakage into activity
stream / audit log storage. Good defense-in-depth.

### 🟡 `.gitignore` has no patterns for secret file types

Current `.gitignore` entries cover build artifacts, editor files, Terraform
state, and Habitat results — but not private key or credential file extensions:

```
# Missing patterns that would add safety-net coverage:
# *.pem       (would need explicit un-ignores for test fixtures)
# *.key
# .env
# *.credentials
```

**Note**: Adding `*.pem` globally would require `!`-exceptions for the test
fixture files. Given the intentional test fixtures, this is a team decision,
not a simple one-liner fix.

### 🟡 Test password literal in integration test helper

```erlang
%% src/oc_erchef/apps/oc_chef_wm/itest/setup_helper.erl:218
{<<"password">>, <<"zuperzecret">>}
```

A literal password in a test fixture. This is acceptable — it is scoped to
integration test setup and will never reach production. It will trigger naive
secret scanners. If a scanner is added to CI, add an inline `# pragma: allowlist secret`
or equivalent suppression comment.

### ✅ oc-id secret_key_base uses Habitat templating

```ruby
# src/oc-id/habitat/config/secret_token.rb
OcId::Application.config.secret_key_base = "{{cfg.secret_key_base}}"
```

Not hardcoded — injected via Habitat config at runtime.

---

## Secrets Handling Architecture (bookshelf)

```
chef-server-ctl (Ruby)
  └─ writes encrypted secrets file (/etc/opscode/private-chef-secrets.json)
       └─ read by chef_secrets Erlang library at OTP app startup
            └─ bksw_conf:credentials/0 fetches access_key_id + secret_access_key
                 └─ injected into #context{} record for per-request use
                      └─ NEVER logged, NEVER serialized to disk
```

This is a well-structured secrets pipeline. No improvements needed for the
Crawl track scope.

---

## Changes Made (Exercise 8 remediation)

### 1. `.gitignore` — secret file pattern section added

```
*.pem  *.key  *.p12  *.pfx  .env  .env.*  *.secret  *.credentials
private_key*  *_rsa  *_dsa  *_ecdsa  *_ed25519
```

Already-tracked `.pem` files (test fixtures, habitat templates) remain tracked —
`.gitignore` only gates new untracked files. Adds a safety net against accidentally
staging future real key material.

### 2. `src/chef-server-ctl/spec/fixtures/pivotal.pem` — test-only header

A plaintext header (valid PEM format; text before `-----BEGIN` is ignored by
PEM parsers) was prepended to make the test-only nature explicit and to provide
an allowlist hint for secret scanners.

### 3. `src/oc_erchef/apps/oc_chef_wm/itest/setup_helper.erl` — fixture comment

Added `%% TEST FIXTURE — not a production credential` comment adjacent to the
`<<"zuperzecret">>` literal.

---

## Recommendations Summary

| Item | Effort | Priority |
|------|--------|----------|
| Add comments to test `.pem` files marking them as test-only | Tiny | Low |
| Add `.pem` allowlist if `gitleaks`/`trufflehog` added to CI | Small | When CI scanner is added |
| Add `# pragma: allowlist secret` to `zuperzecret` line | Tiny | When CI scanner is added |
| Upgrade `omnibus-ctl` from `main` to a tagged release | Medium | See dependencies.md |
