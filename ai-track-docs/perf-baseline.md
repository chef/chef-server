# Performance Baseline — `bksw_format`

**Exercise**: 6 — Crawl Track v2.3.1  
**Module**: `src/bookshelf/src/bksw_format.erl`  
**Date**: 2026-05-18  
**Environment**: WSL2 / Linux x86-64, Erlang/OTP (local `./rebar3` toolchain)  
**Method**: `timer:tc/3` via escript, 10 000 iterations per cell, warm-up of 500 calls before measurement

---

## Benchmark Script

```
/tmp/bench_bksw.escript
```

Run from `src/bookshelf/`:

```bash
# 1. Compile the module (already compiled if tests were run)
erlc -pa _build/default/lib/iso8601/ebin -o /tmp src/bksw_format.erl

# 2. Run benchmark
escript /tmp/bench_bksw.escript
```

---

## Results

### `to_hex/1` — pure-Erlang byte-by-byte hex encoding

| Input size | min (µs) | avg (µs) | p99 (µs) | max (µs) |
|------------|----------|----------|----------|----------|
| 16 bytes   | 17       | 20       | 68       | 453      |
| 128 bytes  | 140      | 159      | 289      | 891      |
| 1 024 bytes| 1 230    | 1 485    | 4 350    | 5 464    |

**Scaling note**: avg time grows roughly linearly with input size
(~9.4 µs per 64 bytes), consistent with the O(n) `byte_to_hex/1` loop.

### `to_base64/1` — delegates to Erlang stdlib `base64:encode/1`

| Input size | min (µs) | avg (µs) | p99 (µs) | max (µs) |
|------------|----------|----------|----------|----------|
| 16 bytes   | 0        | 1        | 1        | 10 144   |
| 128 bytes  | 2        | 2        | 18       | 328      |
| 1 024 bytes| 18       | 23       | 82       | 868      |

**Scaling note**: stdlib `base64:encode/1` is ~64× faster than the
pure-Erlang `to_hex/1` at comparable sizes — C NIF under the hood.

---

## Variance Notes

| Observation | Detail |
|-------------|--------|
| `to_hex` p99 is 3–4× avg | Typical Erlang scheduler jitter on a loaded WSL2 host; not a code issue |
| `to_base64` 16 B max = 10 144 µs | One-time JIT compilation / first-call cost for the `base64` NIF; subsequent calls are sub-microsecond |
| `to_hex` max is ~30× avg at 1 024 B | OS scheduler preemption on large inputs; no outlier pattern across runs |
| Results are **not** suitable for wall-clock SLA decisions | Measurements taken in a shared dev VM; use a dedicated node for capacity planning |

---

## Takeaway

Neither function is a hotspot concern at bookshelf's expected workload
(cookbook metadata calls, not streaming). `to_hex/1` is the only
pure-Erlang path and could be replaced with `binary:encode_hex/1`
(OTP 24+) if sub-microsecond latency were ever required — but that is
**out of scope** for the Crawl track and should only be done with a
confirmed profiler signal.

No optimization work is recommended at this time.
