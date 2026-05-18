#!/usr/bin/env bash
# scripts/test-bookshelf-unit.sh
#
# Runs the bookshelf bksw_format EUnit suite without requiring Postgres,
# a global rebar3 install, or a working C compiler.
#
# Background
# ----------
# 'make all' in src/bookshelf fails on this host with a GCC internal
# compiler error (segfault) when building the jiffy C NIF.  rebar3 eunit
# also triggers that path.  This script bypasses it by compiling only the
# pure-Erlang sources directly with erlc and running eunit via erl -noshell.
#
# Requirements
# ------------
#   - Erlang/OTP on PATH (erl, erlc)
#   - src/bookshelf/_build/default/lib/ populated (run ./rebar3 compile
#     once from src/bookshelf/ — deps fetch succeeds even if jiffy fails)
#
# Usage
#   bash scripts/test-bookshelf-unit.sh          # from repo root
#   bash scripts/test-bookshelf-unit.sh verbose  # extra EUnit output
#
# Exit codes
#   0  all tests passed
#   1  one or more tests failed or compilation error

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BOOKSHELF="$REPO_ROOT/src/bookshelf"
BUILD_DIR="$BOOKSHELF/_build/default/lib"
TMP_DIR="${TMPDIR:-/tmp}/bksw_test_$$"
VERBOSE="${1:-}"

# ── Preflight ──────────────────────────────────────────────────────────────
if ! command -v erlc &>/dev/null; then
    echo "ERROR: erlc not found on PATH" >&2
    exit 1
fi

if [[ ! -d "$BUILD_DIR" ]]; then
    echo "ERROR: $BUILD_DIR not found."
    echo "       Run: cd src/bookshelf && ./rebar3 compile"
    echo "       (compile will fail on jiffy; that is expected — deps still get fetched)"
    exit 1
fi

# ── Setup ──────────────────────────────────────────────────────────────────
mkdir -p "$TMP_DIR"
trap 'rm -rf "$TMP_DIR"' EXIT

export ERL_LIBS="$BUILD_DIR"

# ── Compile source ─────────────────────────────────────────────────────────
echo "==> Compiling bksw_format..."
erlc -I "$BOOKSHELF/include" -o "$TMP_DIR" "$BOOKSHELF/src/bksw_format.erl"

echo "==> Compiling bksw_format_tests..."
erlc -I "$BOOKSHELF/include" -pa "$TMP_DIR" -o "$TMP_DIR" \
    "$BOOKSHELF/test/bksw_format_tests.erl"

# ── Run EUnit ──────────────────────────────────────────────────────────────
EUNIT_OPTS="[verbose]"
[[ -z "$VERBOSE" ]] && EUNIT_OPTS="[]"

echo "==> Running EUnit (bksw_format_tests)..."
erl -noshell \
    -pa "$TMP_DIR" \
    -eval "
        case eunit:test(bksw_format_tests, $EUNIT_OPTS) of
            ok   -> init:stop(0);
            _Err -> init:stop(1)
        end
    "
