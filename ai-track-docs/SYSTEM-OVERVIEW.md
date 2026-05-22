# Chef Server — System Overview

> **Version tracked**: see [`VERSION`](../VERSION)  
> **Audience**: AI assistants, new contributors, on-call engineers

---

## Purpose

Chef Server is the central hub for Chef-based infrastructure automation. It stores node
data, cookbooks, roles, environments, and policy files, and enforces access control for
all Chef clients.

---

## Repository Layout

```
chef-server/
├── src/                  # All service source trees
│   ├── oc_erchef/        # Core REST API (Erlang/OTP)
│   ├── oc_bifrost/       # Authorization service (Erlang/OTP)
│   ├── bookshelf/        # S3-compatible cookbook storage (Erlang/OTP)
│   ├── oc-id/            # OAuth2 / identity provider (Ruby on Rails)
│   ├── chef-server-ctl/  # Management CLI (Ruby)
│   └── nginx/            # Reverse proxy + routing config
├── omnibus/              # Omnibus packaging (git submodule — do not edit directly)
├── habitat/              # Habitat build plans
├── dev/                  # Vagrant-based DVM development environment
├── oc-chef-pedant/       # End-to-end API test suite (Ruby)
├── docs-chef-io/         # Public documentation source
└── ai-track-docs/        # AI-assistant reference docs (this folder)
```

---

## Service Map

| Service | Language | Role | Port (default) |
|---|---|---|---|
| **oc_erchef** | Erlang/OTP | Primary REST API; handles all `/organizations/*` routes | 8000 |
| **oc_bifrost** | Erlang/OTP | ACL / authorization enforcement | 9463 |
| **bookshelf** | Erlang/OTP | Stores cookbook tarballs & checksums | 4321 |
| **oc-id** | Ruby (Rails) | OAuth2 identity provider for web UI & tokens | 9090 |
| **nginx** | C / Lua | TLS termination, routing, rate-limiting | 443 / 80 |
| **PostgreSQL** | — | Persistent data store for all services | 5432 |

---

## Data Flow (request path)

```
Chef Client / Knife
      │  HTTPS
      ▼
   nginx (TLS termination)
      │
      ├──▶ oc_erchef  (API logic)
      │         │
      │         ├──▶ oc_bifrost  (authz check)
      │         ├──▶ bookshelf   (cookbook blobs)
      │         └──▶ PostgreSQL  (node / metadata)
      │
      └──▶ oc-id  (OAuth2 flows)
```

---

## Key Abstractions

- **Organization** — top-level multi-tenant boundary; almost every API call is scoped to an org.
- **Actor** — a user or client (node) that authenticates via RSA-signed requests.
- **ACE / Bifrost object** — every Chef resource (node, cookbook, role …) is backed by a Bifrost
  access-control entry.
- **Cookbook** — versioned bundle of recipes, templates, and files stored in bookshelf.

---

## Technology Notes

- Erlang services use **rebar3**, OTP supervision trees, **sqerl** (SQL), and **pooler**
  (connection pooling).
- Database schema is versioned with **sqitch** migrations found in each service's
  `schema/` subdirectory.
- Ruby tooling uses **Bundler**; Rails app (`oc-id`) follows Rails 7.x conventions.
- Omnibus is a **git submodule** — never edit files under `omnibus/` directly; open PRs
  against the omnibus repo and update the submodule ref.

---

## Authentication Model

Requests are signed with **RSA-SHA256** using the Chef request-signing protocol (`mixlib-authn`).
The server validates the signature against the actor's stored public key and then delegates
permission checks to oc_bifrost.

---

## Language & Framework Summary (Exercise 1)

| Language | Services | Framework / Build Tool |
|---|---|---|
| **Erlang/OTP** | oc_erchef, oc_bifrost, bookshelf | rebar3, webmachine, sqerl, pooler |
| **Ruby** | chef-server-ctl, oc-id, oc_bifrost schema | Rails 7, Thor, Bundler |
| **SQL** | All services (schema migrations) | sqitch |
| **Lua / nginx conf** | nginx, openresty-noroot | OpenResty |

---

## Entry Points

| Service | Entry Point Path | Notes |
|---|---|---|
| oc_erchef | `src/oc_erchef/src/oc_erchef.app.src` | Composed of 10 sub-apps: `oc_chef_wm`, `chef_db`, `chef_objects`, `depsolver`, `chef_index`, `chef_telemetry`, `chef_license`, `data_collector`, `oc_chef_authz`, `chef_test` |
| oc_bifrost | `src/oc_bifrost/apps/bifrost/` | Single OTP app |
| bookshelf | `src/bookshelf/src/bksw_app.erl` | Entry via `bksw_app` application callback |
| oc-id | `src/oc-id/` | Rails app; `config/application.rb` is the Rails entry point |
| chef-server-ctl | `src/chef-server-ctl/chef-server-ctl.gemspec` | Thor-based CLI |
| nginx | `src/nginx/` | Config templates rendered by omnibus |

---

## Test Approach

| Service tier | Framework | Files | Run command |
|---|---|---|---|
| Erlang unit | EUnit (`*_tests.erl`) | 45 files | `rebar3 eunit` |
| Erlang integration | Common Test (`*_SUITE.erl`) | 19 files | `rebar3 ct` |
| Ruby | RSpec (`*_spec.rb`) | 47 files | `bundle exec rspec` |
| End-to-end API | oc-chef-pedant | full suite | `bundle exec rspec` (from `oc-chef-pedant/`) |

All Erlang services have a `Makefile` with a `make all` target that runs clean → compile → eunit → dialyzer.

---

## Chosen Low-Risk Module (Exercise 1)

**`src/bookshelf/src/bksw_format.erl`**

### What it does
Pure formatting utility for bookshelf responses. Exports four stateless functions:

| Function | Purpose |
|---|---|
| `to_date/1` | Formats a datetime tuple as an ISO 8601 string |
| `to_base64/1` | Base64-encodes a binary |
| `to_hex/1` | Converts a binary to a lowercase hex string |
| `to_etag/1` | Wraps a hex digest in ETag quotes (`"abc123"`) |

### Why it is low risk

- **46 lines total** — entire module fits on one screen
- **Pure functions** — no side effects, no process state, no supervision tree involvement
- **No auth, no DB, no HTTP** — cannot accidentally break security, data integrity, or API contracts
- **Self-contained** — zero internal dependencies beyond stdlib (`base64`, `io_lib`, `string`) and the `iso8601` library
- **Easily testable** — input → output; no infrastructure required to run or verify tests
- A bug here affects only the *formatting* of dates/ETags in bookshelf HTTP responses — not data loss, not authentication failure, not authorization bypass
- Referenced in `src/bookshelf/test/bksw_sec_tests.erl`, so there is an existing test harness to extend

### What kinds of changes are safe here

- Adding/improving EUnit tests
- Improving `to_hex` readability (the current implementation uses a list comprehension with `io_lib:format` — it could be made more idiomatic)
- Adding a `-spec` type annotation for each exported function
- Adding a `to_mime_type/1` or similar utility if bookshelf needs it in future exercises

---

## Further Reading

- [`build-test.md`](./build-test.md) — how to build, run, and test each service
- [`architecture.mmd`](./architecture.mmd) — Mermaid component diagram
- [`dev/README.md`](../dev/README.md) — DVM local development guide
- [`CONTRIBUTING.md`](../CONTRIBUTING.md) — contribution workflow
