# Extending `bksw_format`

**Module**: `src/bookshelf/src/bksw_format.erl`  
**Test file**: `src/bookshelf/test/bksw_format_tests.erl`

---

## What this module is

`bksw_format` is a small, pure utility module. It converts internal Erlang
values into the string/binary representations required by bookshelf's
S3-compatible HTTP API. All functions are stateless — no processes, no
database, no side effects.

---

## When to add a function here

Add a function to `bksw_format` when you need to:

- Format an Erlang value into a specific HTTP header or response body string
- Centralise a conversion used in more than one place in bookshelf
- Keep formatting logic out of resource/request handler modules (`bksw_wm_*`)

**Do not add** functions that perform I/O, call external services, or have
side effects — those belong elsewhere in the bookshelf supervision tree.

---

## How to add a new formatting function

### 1. Write the function with a `-spec`

```erlang
%% @doc One-line description of what this formats.
%%
%% More detail if needed — input shapes, edge cases, examples.
-spec to_foo(InputType()) -> OutputType().
to_foo(Value) ->
    %% implementation
    Value.
```

Place it in the `%% API functions` section, alphabetically or grouped by
concern. Export it in the `-export` list at the top.

### 2. Add a test in `bksw_format_tests.erl`

Follow the existing pattern — one test function per behaviour, with a
descriptive name ending in `_test`:

```erlang
to_foo_converts_value_correctly_test() ->
    ?assertEqual(expected_output, bksw_format:to_foo(known_input)).

to_foo_handles_edge_case_test() ->
    ?assertEqual(edge_output, bksw_format:to_foo(edge_input)).
```

Always include at least:
- A **happy-path** test with a known input/output pair
- An **edge case** (empty binary, `undefined`, zero, etc.) if applicable

### 3. Run tests

```bash
cd src/bookshelf/
./rebar3 eunit
```

All tests must pass before committing.

---

## Current exports reference

| Function | Input | Output | Notes |
|---|---|---|---|
| `to_date/1` | `undefined` \| `{datetime, D}` \| `D` | `binary()` | Returns epoch string for `undefined` |
| `to_base64/1` | `binary()` | `string()` | Flat base64 string |
| `to_hex/1` | `binary()` | `string()` | Lowercase, zero-padded, two chars per byte |
| `to_etag/1` | `binary()` \| `string()` | `string()` | Hex-encodes binaries first, then wraps in `"..."` |

---

## Example: adding `to_content_md5/1`

Suppose bookshelf needs to format a raw MD5 digest binary as the
`Content-MD5` header value (base64-encoded):

```erlang
%% @doc Encode a raw MD5 digest binary as a Content-MD5 header string.
-spec to_content_md5(binary()) -> string().
to_content_md5(Digest) ->
    to_base64(Digest).
```

Test:

```erlang
to_content_md5_encodes_md5_digest_test() ->
    %% MD5 of empty string is d41d8cd98f00b204e9800998ecf8427e (hex)
    RawMd5 = <<16#d4,16#1d,16#8c,16#d9,16#8f,16#00,16#b2,16#04,
               16#e9,16#80,16#09,16#98,16#ec,16#f8,16#42,16#7e>>,
    ?assertEqual("1B2M2Y8AsgTpgAmY7PhCfg==", bksw_format:to_content_md5(RawMd5)).
```

---

## Keeping the module healthy

- Keep all functions **pure** — no side effects
- Keep the module **small** — if it grows beyond ~100 lines consider splitting
- Keep `-spec` attributes on every function
- Keep test coverage at **100 %** — this module is simple enough to achieve it
