# Chef Server — Build & Test Reference

> Quick reference for building and testing every service tier.  
> All commands assume you are inside the relevant service directory unless noted.

---

## Prerequisites

| Tool | Purpose |
|---|---|
| Erlang/OTP ≥ 25 | Erlang services |
| rebar3 | Erlang build & test |
| Ruby ≥ 3.1 | Ruby services & tooling |
| Bundler | Ruby dependency management |
| PostgreSQL | Integration / CT tests |
| Vagrant + VirtualBox | Full dev environment (DVM) |
| Docker / docker-compose | Container-based smoke tests |

---

## Development VM (DVM) — Recommended Start

```bash
cd dev/
vagrant up                              # provision VM (~first run: several minutes)
vagrant ssh
sudo -i
dvm load oc_erchef                      # symlink source for hot-reload
dvm start oc_erchef                     # start service inside VM
```

Reload Erlang modules without restarting:

```bash
dvm reload oc_erchef
```

---

## Erlang Services (`oc_erchef`, `oc_bifrost`, `bookshelf`)

### Full build (clean → compile → eunit → dialyzer)

```bash
cd src/<service>/
make all
```

> **bookshelf note**: `make all` also runs `elvis` style check and `xref`.
> On this machine, `make all` may fail with a GCC segfault while compiling
> the `jiffy` C NIF — a pre-existing system compiler bug unrelated to our code.
> Use `./rebar3 eunit` directly to run the Erlang-only test path cleanly.

### Granular steps

```bash
./rebar3 clean
./rebar3 compile
./rebar3 eunit                          # unit tests
./rebar3 dialyzer                       # type analysis
make ct                                 # Common Test integration suite (requires PostgreSQL)
```

> Each service ships its own `rebar3` binary at `src/<service>/rebar3`.
> Use `./rebar3` rather than a global `rebar3` to ensure the correct version.

### bookshelf — exact verified commands (Exercise 2 baseline)

```bash
cd src/bookshelf/

./rebar3 eunit                          # run all EUnit tests including bksw_format_tests
./rebar3 eunit --module bksw_format_tests  # run only the format tests
./rebar3 dialyzer                       # type checking
make ct                                 # integration tests (requires PostgreSQL running)
```

**Verified test output (all passing):**

```
======================== EUnit ========================
module 'bksw_format_tests'
  bksw_format_tests: to_hex_produces_lowercase_hex_string_test...[0.035 s] ok
  bksw_format_tests: to_hex_empty_binary_produces_empty_string_test...ok
  bksw_format_tests: to_base64_encodes_binary_test...[0.013 s] ok
  bksw_format_tests: to_etag_wraps_hex_in_quotes_test...ok
  bksw_format_tests: to_date_undefined_returns_epoch_test...ok
  [done in 0.063 s]
=======================================================
  All 5 tests passed.
```

### Code style

```bash
../../scripts/elvis rock                # from src/<service>/ directory
```

### Coverage report

```bash
./rebar3 cover
# HTML report appears in _build/test/cover/
```

---

## Ruby Services & Tooling

### `oc-id` (Rails OAuth2 provider)

```bash
cd src/oc-id/
bundle install
bundle exec rails db:setup              # first time
bundle exec rails db:migrate
bundle exec rspec                       # test suite
bundle exec rails server                # dev server on :3000
```

### `chef-server-ctl` (management CLI)

```bash
cd src/chef-server-ctl/
bundle install
bundle exec rspec
```

### Style

```bash
bundle exec rubocop                     # Ruby style (where .rubocop.yml present)
```

---

## End-to-End / API Tests

```bash
cd oc-chef-pedant/
bundle install
bundle exec rspec spec/                 # requires a running Chef Server stack
```

Run against the Vagrant VM:

```bash
cd dev/
vagrant ssh -c "sudo -i chef-server-ctl test"
```

---

## Docker Compose (smoke test stack)

```bash
# From repo root
docker-compose up -d
docker-compose exec chef-server-ctl chef-server-ctl test
docker-compose down
```

---

## Omnibus Packaging

> `omnibus/` is a **git submodule** — do not edit files there.  
> To build a package, work against the omnibus repo directly.

```bash
cd omnibus/
make dev-build                          # requires omnibus toolchain
```

---

## Habitat Builds

```bash
# Single service
hab pkg build src/oc_erchef

# All services
./habitat_pkgs_build.sh
```

---

## CI / Quality Gates

### GitHub Actions (`.github/workflows/ci-main-pull-request-stub.yml`)

The repo's GitHub Actions CI runs on PRs to `main`, `develop`, and `release/**`.
It delegates to a central `chef/common-github-actions` reusable workflow.

**What CI does run:**

| Scan | Tool | Fail threshold |
|------|------|---------------|
| Secret scanning | Trufflehog | Any secret found |
| Dependency CVEs | Trivy + Grype | High / Critical |
| SAST | BlackDuck Polaris | High / Critical |
| SCA / SBOM | BlackDuck SCA | Blocker / Critical / Major |
| Code quality | SonarQube | Configurable |

**What CI does NOT run (as of this writing):**

```yaml
build: false
unit-tests: false
```

Erlang unit tests, dialyzer, elvis, and Ruby specs are **not executed by CI**.
They must be run locally before pushing. See local script below.

### Local unit test script (bookshelf — Exercise 10)

A reliable local runner is provided that bypasses the GCC/jiffy segfault:

```bash
# From repo root — run all 8 bksw_format_tests (quiet)
bash scripts/test-bookshelf-unit.sh

# Verbose EUnit output
bash scripts/test-bookshelf-unit.sh verbose
```

**Script**: `scripts/test-bookshelf-unit.sh`  
**Requirement**: `_build/default/lib/` must be populated (run `./rebar3 compile`
once from `src/bookshelf/` — it fails on jiffy but fetches all deps first).

### Full local quality check (bookshelf)

```bash
cd src/bookshelf/
./rebar3 eunit            # unit tests (or use the script above if GCC segfaults)
./rebar3 dialyzer         # type analysis
../../scripts/elvis rock  # style check
make ct                   # CT integration (requires PostgreSQL)
```

| Check | Command | Must pass? |
|-------|---------|------------|
| Erlang unit tests | `rebar3 eunit` or `scripts/test-bookshelf-unit.sh` | ✅ |
| Erlang dialyzer | `rebar3 dialyzer` | ✅ |
| Erlang CT integration | `rebar3 ct` | ✅ (needs PG) |
| Ruby specs | `bundle exec rspec` | ✅ |
| Elvis style | `../../scripts/elvis rock` | ✅ |
| Pedant API tests | `bundle exec rspec` (oc-chef-pedant) | ✅ (needs stack) |

Coverage target: **≥ 85 %** line coverage for all critical modules.

---

## Common Troubleshooting

| Symptom | Fix |
|---|---|
| `rebar3 compile` fails on missing dep | Run `./rebar3 get-deps` then retry |
| CT tests can't connect to PostgreSQL | Ensure pg is running; check `test.config` for DB params |
| `bundle install` fails with native ext errors | Install OS dev headers (`libpq-dev`, `libxml2-dev`) |
| Vagrant VM times out on `vagrant up` | Increase VirtualBox RAM; see `dev/Vagrantfile` |
| Hot-reload not picking up changes | Confirm `dvm load` was run; check symlinks in `/srv/` |
| `make all` in bookshelf crashes with `cc1: internal compiler error: Segmentation fault` | GCC system bug compiling jiffy C NIF; use `./rebar3 eunit` to run Erlang tests directly |
| `rebar3: command not found` | Each service ships its own binary; use `./rebar3` from inside `src/<service>/` |

<!-- ex11: PR #4186 updated with review focus, risks, verification, rollback, commit message proposals -->
