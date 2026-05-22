# Feature Toggles

**Exercise**: 13 — Crawl Track v2.3.1  
**Date**: 2026-05-18

---

## Toggle: `hex_encoding_case` — hex letter case in `bksw_format:to_hex/1`

### What it controls

The case of hex letters produced by `bksw_format:to_hex/1`, which flows into:

- S3 ETag headers (via `bksw_format:to_etag/1`)
- Request IDs (via `bksw_req:generate_id/0` → `to_base64`, not affected)
- XML listing responses (via `bksw_xml:object_record/1`)

### Why it exists

The S3 specification does not mandate lowercase hex for ETags, but most AWS
tooling and clients produce and compare lowercase. A small number of S3-compatible
clients compare ETags case-insensitively or require uppercase. This toggle allows
operators to switch case without a code deploy.

### Configuration

| Key | Value | Behavior |
|-----|-------|----------|
| `hex_encoding_case` | `lowercase` (**default**) | `"0aff"` — S3-standard, existing behavior |
| `hex_encoding_case` | `uppercase` | `"0AFF"` — for compatibility with strict uppercase clients |

### How to enable

**At runtime** (takes effect immediately, no restart required):

```erlang
%% From an Erlang remote shell (remsh):
application:set_env(bookshelf, hex_encoding_case, uppercase).

%% To revert:
application:set_env(bookshelf, hex_encoding_case, lowercase).
%% or
application:unset_env(bookshelf, hex_encoding_case).
```

**In the bookshelf `sys.config`** (requires restart):

```erlang
[
  {bookshelf, [
    {hex_encoding_case, uppercase},
    %% ... other config
  ]}
].
```

### Implementation

```erlang
%% src/bookshelf/src/bksw_format.erl — to_hex/1
to_hex(Bin) when is_binary(Bin) ->
    Hex = lists:flatten([byte_to_hex(B) || <<B>> <= Bin]),
    case application:get_env(bookshelf, hex_encoding_case, lowercase) of
        uppercase -> string:to_upper(Hex);
        _         -> Hex
    end.
```

**Design decisions**:
- `application:get_env/3` is called once per `to_hex/1` invocation (not per byte)
- `string:to_upper/1` is only called on the uppercase path (default is zero-cost)
- Any value other than `uppercase` (including unset, `lowercase`, or a typo)
  falls through to the default lowercase behavior — fail-safe

### Tests

Two new tests in `src/bookshelf/test/bksw_format_tests.erl`:

```erlang
%% Toggle ON: uppercase mode
to_hex_uppercase_toggle_produces_uppercase_test() ->
    application:set_env(bookshelf, hex_encoding_case, uppercase),
    try
        ?assertEqual("0A1B2C3D", bksw_format:to_hex(<<16#0a, 16#1b, 16#2c, 16#3d>>))
    after
        application:unset_env(bookshelf, hex_encoding_case)  %% always clean up
    end.

%% Toggle OFF: default lowercase when env is unset
to_hex_default_is_lowercase_when_toggle_unset_test() ->
    application:unset_env(bookshelf, hex_encoding_case),
    ?assertEqual("0a1b2c3d", bksw_format:to_hex(<<16#0a, 16#1b, 16#2c, 16#3d>>)).
```

The `try...after` block ensures the test environment is always cleaned up,
preventing toggle state from leaking into subsequent tests.

### Risks & Limitations

| Concern | Note |
|---------|------|
| ETag comparison | If ETags stored in DB are lowercase and the toggle is flipped to uppercase, ETag checks on in-flight downloads will mismatch. Only change this toggle before or after a maintenance window, not during active uploads. |
| `to_etag/1` affected | Since `to_etag/1` calls `to_hex/1` internally, ETags in XML listings and HTTP headers will also change case. |
| Not persisted across restarts | Unless set in `sys.config`, `application:set_env` changes are lost on restart. |
| No audit trail | There is no log entry when the toggle changes. Operators should log the change manually. |

### What is NOT toggled

- `to_base64/1` — base64 alphabet is case-defined by RFC 4648; no toggle needed
- `to_etag/1` — format (quotes) is fixed by S3 protocol; only hex case is toggled
- `to_date/1` — ISO 8601 format; no case concern
