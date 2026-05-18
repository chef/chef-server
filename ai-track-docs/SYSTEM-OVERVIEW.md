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

## Further Reading

- [`build-test.md`](./build-test.md) — how to build, run, and test each service
- [`architecture.mmd`](./architecture.mmd) — Mermaid component diagram
- [`dev/README.md`](../dev/README.md) — DVM local development guide
- [`CONTRIBUTING.md`](../CONTRIBUTING.md) — contribution workflow
