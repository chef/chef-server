# Static Analysis — Exercise 14

**Date**: 2026-05-18  
**Scope**: `src/bookshelf/src/*.erl` — 20 modules  
**Tools**: Elvis (style), Dialyzer (type / flow)

---

## Elvis (style linter)

### Config

`src/bookshelf/elvis.config` — 9 rules applied to all `src/*.erl` files:

| Rule | What it checks |
|------|----------------|
| `macro_names` | Macros must be UPPER_CASE |
| `no_if_expression` | No `if` expressions (use `case` instead) |
| `no_debug_call` | No `io:format`, `erlang:display` (except `bksw_app`) |
| `no_nested_try_catch` | No `try` inside another `try` |
| `no_tabs` | No tab characters |
| `no_trailing_whitespace` | No trailing spaces |
| `operator_spaces` | Spaces around operators |
| `used_ignored_variable` | No `_Var` that is actually used |
| `variable_naming_convention` | CamelCase variable names |

### Result

```
# src/bksw_app.erl [OK]
# src/bksw_cleanup_task.erl [OK]
# src/bksw_conf.erl [OK]
# src/bksw_format.erl [OK]          ← our practice module
# src/bksw_io.erl [OK]
# src/bksw_io_names.erl [OK]
# src/bksw_req.erl [OK]
# src/bksw_sec.erl [OK]
# src/bksw_sql.erl [OK]
# src/bksw_sup.erl [OK]
# src/bksw_util.erl [OK]
# src/bksw_webmachine_sup.erl [OK]
# src/bksw_wm_base.erl [OK]
# src/bksw_wm_bucket.erl [OK]
# src/bksw_wm_index.erl [OK]
# src/bksw_wm_object.erl [OK]
# src/bksw_wm_sql_bucket.erl [OK]
# src/bksw_wm_sql_index.erl [OK]
# src/bksw_wm_sql_object.erl [OK]   ← Ex9 req_id changes
# src/bksw_xml.erl [OK]

Exit 0 — 0 violations across all 20 files
```

All Exercise 3–13 changes to `bksw_format.erl` and `bksw_wm_sql_object.erl`
pass every Elvis rule.

**How to run**:
```bash
cd src/bookshelf
../../scripts/elvis rock --config elvis.config -V
```

---

## Dialyzer (type / success-typing analysis)

### Setup

No pre-built PLT was present in this environment. A minimal OTP PLT was built
from `erts`, `stdlib`, and `kernel` for targeted module analysis:

```bash
dialyzer --build_plt --output_plt /tmp/otp_base.plt --apps erts stdlib kernel
# 1m7s; emits only qlc_pt.erl compile:option/0 warning (OTP stdlib internal)
```

### `bksw_format.erl` — clean

**Without iso8601 dep** (minimal PLT only):
```
Unknown functions: iso8601:format/1 (src/bksw_format.erl:45:5)
Exit 2 (warnings-as-exit)
```
→ Expected: `iso8601` is a 3rd-party dep not in the minimal PLT. Not a real defect.

**With iso8601 dep** added to analysis:
```bash
dialyzer --plt /tmp/otp_base.plt \
  _build/default/lib/iso8601/ebin \
  /tmp/bksw_format.beam
# done (passed successfully)
# Exit 0 — no warnings
```

All `-spec` attributes are consistent with actual return types:

| Function | Spec return | Actual return |
|----------|------------|---------------|
| `to_date/1` | `binary()` | `binary()` ✓ |
| `to_base64/1` | `string()` | `string()` (base64:encode_to_string) ✓ |
| `to_hex/1` | `string()` | `string()` (list of chars from byte_to_hex) ✓ |
| `to_etag/1` | `string()` | `string()` ✓ |
| `byte_to_hex/1` | `string()` | `string()` ✓ |

The Ex13 toggle (`string:to_upper/1` applied to a `string()`) returns `string()` — no
type widening or spec violation.

### `bksw_wm_sql_object.erl` — "unknown functions" only (expected)

When analyzed in isolation (without the full bookshelf PLT), all warnings are
"Unknown functions" — calls to sibling modules (`bksw_format`, `bksw_sql`,
`bksw_req`, `bksw_util`, `bksw_wm_base`) and the `crypto` OTP app not present
in the minimal PLT. These are **not defects**.

One pre-existing finding from the pulled-in `mochiweb` library:
```
mochiweb_http.erl:79:7: The pattern
    {'error', {'already_started', _Pid}} can never match the type {'ok', pid()}
```
→ Pre-existing in mochiweb v2.x; not in our code; not introduced by Ex9 changes.

---

## Summary

| Tool | Files checked | Violations | Notes |
|------|--------------|------------|-------|
| Elvis | 20 `src/*.erl` | **0** | All 9 rules pass |
| Dialyzer (`bksw_format`) | 1 | **0** (with dep) | `-spec` attrs consistent |
| Dialyzer (`bksw_wm_sql_object`) | 1 | 0 in our code | "Unknown fn" warnings expected |

---

## Gaps & Recommendations

| Item | Priority | Note |
|------|----------|------|
| Full bookshelf PLT | Medium | `rebar3 dialyzer` builds a complete PLT including all deps; blocked by jiffy NIF segfault in this env |
| CI integration | High | Neither Elvis nor Dialyzer run in CI (see `build-test.md`); adding them would catch regressions automatically |
| `elvis.config` coverage | Low | `test/*.erl` not linted; consider adding a `test` dir stanza |
| Dialyzer `unknown_function` allowlist | Low | A `.dialyzer.ignore` file (or `rebar3 dialyzer` config) would suppress noise from cross-module unknowns |

---

## How to Run (locally)

```bash
# Elvis — style check all bookshelf modules
cd src/bookshelf
../../scripts/elvis rock --config elvis.config -V

# Dialyzer — type check bksw_format.erl (requires pre-built PLT)
ERL_LIBS=_build/default/lib erlc +debug_info -I include -o /tmp src/bksw_format.erl
dialyzer --plt /tmp/otp_base.plt _build/default/lib/iso8601/ebin /tmp/bksw_format.beam

# rebar3 dialyzer (full project; requires jiffy NIF build to work)
cd src/bookshelf && ./rebar3 dialyzer
```
