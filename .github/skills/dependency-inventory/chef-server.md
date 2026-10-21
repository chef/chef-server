# Chef Server repository extension

Apply this checklist only when the target is `chef/chef-server`. Verify all
paths and current line numbers against the checkout before reporting; the
references below are discovery locations, not dependency claims by themselves.

## Components and dependency files

- The root `README.md` identifies the service components and technologies.
- Erlang/Rebar services:
  - `src/oc_erchef/rebar.config` and `src/oc_erchef/rebar.lock`
  - `src/oc_bifrost/rebar.config` and `src/oc_bifrost/rebar.lock`
  - `src/bookshelf/rebar.config` and `src/bookshelf/rebar.lock`
  - Search `src/oc_erchef/apps/` for additional app-level `rebar.config` and
    `rebar.lock` files, including test and nested app dependencies.
- Ruby/Bundler:
  - `src/oc-id/Gemfile` and `src/oc-id/Gemfile.lock`
  - `src/chef-server-ctl/Gemfile` and `src/chef-server-ctl/Gemfile.lock`
  - `oc-chef-pedant/Gemfile` and `oc-chef-pedant/Gemfile.lock`
  - `src/oc_bifrost/Gemfile` and the manifests/lockfiles under
    `src/oc_bifrost/oc-bifrost-pedant/`
  - `src/oc_erchef/Gemfile_habitat` and `src/oc_erchef/Gemfile_habitat.lock`
  - Search `src/oc_erchef/apps/` for nested Gemfiles, such as the
    `chef_objects/priv/depselector_rb/` bundle.
- Habitat packages:
  - Inspect `habitat/plan.sh` under `src/bookshelf`,
    `src/chef-server-ctl`, `src/oc-id`, `src/oc_bifrost`, `src/oc_erchef`,
    `src/nginx`, and `src/openresty-noroot`.
  - In each plan, report `pkg_deps` separately from `pkg_build_deps`;
    identify package origin and version constraints as written.
  - Check plans for direct upstream archive URLs, checksums, and install-time
    downloads, not only Habitat packages.
  - Check `.bldr.toml`, `.expeditor/`, and `habitat_pkgs_build.sh` for package
    plan mappings, channels, and build behavior.
- Packaging and external components:
  - `.gitmodules` declares the `omnibus/` configuration submodule and its
    upstream repository.
  - `docker-compose.yml` declares development runtime images and service
    bindings. Distinguish these images and external services from libraries
    used to compile Chef Server.
  - Inspect `omnibus/` only if it is initialized and available; otherwise
    report that its contents could not be verified.
  - The README mentions related projects maintained elsewhere, including
    `knife-ec-backup` and `knife-opc`. Present these as related external
    projects, not as build dependencies unless repository evidence shows they
    are fetched or required by the build/runtime.

## Reporting guidance

Use separate table groups for Ruby gems, Erlang/Rebar dependencies, Habitat
runtime packages, Habitat build packages, upstream source archives, submodules,
and container/runtime images when present. State the service or repository area
for every row.

For direct dependencies, link to the precise manifest lines that declare the
package, URL, constraint, or source. Link lockfiles as whole files when
summarizing their resolved dependency inventories; if reporting a particular
resolved entry, use its exact lines where practical. For Erlang dependencies,
the manifest may name a branch or tag while the lockfile records a commit
revision; distinguish those facts and cite both. For Ruby dependencies, cite
the Gemfile declaration and lockfile resolution as needed. Do not call a
dependency bundled just because a lockfile is committed.

Describe Chef Server as a monorepo for its core services, but do not describe
it as self-contained: confirm external sources and runtime inputs in the
current manifests before making the verdict.
