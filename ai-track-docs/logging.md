# Structured Logging Guide — Bookshelf

**Exercise**: 9 — Crawl Track v2.3.1  
**Scope**: `src/bookshelf/`  
**Date**: 2026-05-18

---

## Logging Stack

Bookshelf uses **lager** (via parse transform) with a thin macro layer defined in
`src/internal.hrl`:

```erlang
%% info/error → error_logger (visible via SASL in Common Test)
-define(LOG_INFO(X, Y),  error_logger:info_msg(X, Y)).
-define(LOG_ERROR(X, Y), error_logger:error_msg(X, Y)).

%% debug → direct lager call (requires lager app running)
-define(LOG_DEBUG(X, Y), lager:debug(X, Y)).
```

High-severity events (checksum mismatches, upload failures) use `lager:error/2`
directly, which routes through lager's formatters and sinks.

---

## The `req_id` Convention

Every bookshelf HTTP request gets a unique ID assigned at the webmachine layer:

```erlang
%% set by opscoderl_wm:read_req_id in bksw_wm_base
#context{reqid = ReqId}
```

**The req_id must appear in every error and warning log line** emitted during
request processing. This is the primary trace key for correlating nginx access
logs, lager error logs, and external client errors.

Format: `"req_id=~s ..."` (binary-safe string format with `~s`)

---

## Changes Made in Exercise 9

Three error log lines in `bksw_wm_sql_object.erl` lacked `req_id` context despite
having `#context{}` in scope. These are the highest-value lines to fix because
they represent integrity failures (checksum/MD5 mismatches) that operators need
to trace to a specific request.

### Before → After

**1. Download checksum mismatch** (`send_streamed_body/1`):
```erlang
%% Before — no trace context, no structured fields
lager:error("checksum mismatch on download: expected: ~p; sent: ~p",
            [ShaExpected, S])

%% After — req_id added; field names are key=value tokens (grep-friendly)
lager:error("req_id=~s checksum mismatch on download: expected=~p sent=~p",
            [ReqId, ShaExpected, S])
```
The function pattern was also updated to extract `reqid = ReqId` directly:
```erlang
send_streamed_body(#context{..., reqid = ReqId}) -> ...
```

**2. Upload Content-MD5 mismatch** (`write_streamed_body/3`):
```erlang
%% Before
lager:error("Mismatch between Content-MD5 and actual content. Content-MD5: ~p; Actual: ~p",
            [RequestMd5, HashMd5])

%% After — req_id from Ctx1 in scope; consistent key=value format
lager:error("req_id=~s content-md5 mismatch on upload: content-md5=~p actual=~p",
            [Ctx1#context.reqid, RequestMd5, HashMd5])
```

**3. Unknown upload response** (`maybe_upload/3`):
```erlang
%% Before — error_logger (lower visibility), no trace context
error_logger:error_msg("maybe_upload unknown response: ~p~n", [Error])

%% After — promoted to lager:error; req_id added; function pattern updated
maybe_upload(Rq, #context{reqid = ReqId} = Ctx, Error) ->
    lager:error("req_id=~s upload failed with unknown response: ~p", [ReqId, Error])
```

---

## Logging Best Practices for Bookshelf

### Always include `req_id` in request-scoped logs

```erlang
%% Good — traceable to a specific request
lager:error("req_id=~s operation failed: reason=~p", [ReqId, Reason])

%% Bad — unattributed, cannot be correlated
lager:error("operation failed: ~p", [Reason])
```

### Use `key=value` field format in message strings

Structured `key=value` tokens let operators grep and parse logs without a
dedicated log parser:

```bash
# Find all errors for a specific request
grep 'req_id=abc-123' /var/log/opscode/bookshelf/current

# Find all checksum mismatches
grep 'checksum mismatch on download' /var/log/opscode/bookshelf/current
```

### Choose the right level

| Level | When to use | Macro/call |
|-------|------------|------------|
| `debug` | Per-request internals, normal control flow | `?LOG_DEBUG/2` |
| `info` | Startup, configuration, lifecycle events | `?LOG_INFO/2` |
| `error` | Integrity failures, unexpected states, security events | `lager:error/2` |

### Prefer pattern extraction over record access in function heads

```erlang
%% Preferred — reqid visible in the function head
send_streamed_body(#context{..., reqid = ReqId}) -> ...

%% Acceptable when reqid is needed mid-function
lager:error("req_id=~s ...", [Ctx#context.reqid, ...])
```

---

## Modules with Good Existing Logging

| Module | Pattern used | Notes |
|--------|-------------|-------|
| `bksw_io.erl` | `?LOG_DEBUG`, `?LOG_ERROR` macros | Good coverage of filesystem ops |
| `bksw_sec.erl` | `?LOG_DEBUG` with `req_id=~p` | Correct trace pattern for auth failures |
| `bksw_sup.erl` | `lager:info` | Startup lifecycle events only |
| `bksw_cleanup_task.erl` | `lager:debug` | Background task logging |

---

## What Was Not Changed

`bksw_format.erl` has no logging — it is a pure stateless utility module and
should remain so. Logging belongs at the I/O and request-handling boundaries,
not inside encoding/formatting primitives.
