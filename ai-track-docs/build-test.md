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

### Granular steps

```bash
rebar3 clean
rebar3 compile
rebar3 eunit                            # unit tests
rebar3 dialyzer                         # type analysis
rebar3 ct                               # Common Test integration suite
```

### Code style

```bash
./scripts/elvis rock                    # from repo root, or:
rebar3 as lint lint                     # where configured
```

### Coverage report

```bash
rebar3 cover
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

| Check | Command | Must pass? |
|---|---|---|
| Erlang unit tests | `rebar3 eunit` | ✅ |
| Erlang dialyzer | `rebar3 dialyzer` | ✅ |
| Erlang CT integration | `rebar3 ct` | ✅ |
| Ruby specs | `bundle exec rspec` | ✅ |
| Elvis style | `./scripts/elvis rock` | ✅ |
| Pedant API tests | `bundle exec rspec` (oc-chef-pedant) | ✅ |

Coverage target: **≥ 85 %** line coverage for all critical modules.

---

## Common Troubleshooting

| Symptom | Fix |
|---|---|
| `rebar3 compile` fails on missing dep | Run `rebar3 get-deps` then retry |
| CT tests can't connect to PostgreSQL | Ensure pg is running; check `test.config` for DB params |
| `bundle install` fails with native ext errors | Install OS dev headers (`libpq-dev`, `libxml2-dev`) |
| Vagrant VM times out on `vagrant up` | Increase VirtualBox RAM; see `dev/Vagrantfile` |
| Hot-reload not picking up changes | Confirm `dvm load` was run; check symlinks in `/srv/` |
