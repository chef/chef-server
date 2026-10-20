# Chef Server dependencies

## Is this repository self-contained?

No. This is a monorepo for Chef Server's main components, but it does not vendor
all the dependencies needed to build or run them. The component source and build
definitions are here; build and runtime workflows download language libraries,
Habitat packages, and external source archives, and use a separate Omnibus
configuration repository. Lockfiles pin many resolved dependencies, but do not
contain the dependency source code itself.

## Dependency sources and inventory

| Dependency type | Where dependencies are declared and resolved | Source |
| --- | --- | --- |
| Erlang libraries | Rebar manifests (`rebar.config`) and lockfiles (`rebar.lock`) | Mostly Git repositories, commonly GitHub |
| Ruby gems | Bundler manifests (`Gemfile`, `Gemfile_habitat`) and lockfiles | RubyGems at `https://rubygems.org` and, for some gems, Git repositories |
| Habitat packages | Each package's `habitat/plan.sh` | Habitat package origins, including `core` and `chef` |
| External source archives | Habitat build plans | Upstream download URLs, with checksums where specified |
| Omnibus configuration | Git submodule at `omnibus/` | `https://github.com/chef/chef-server-omnibus-config.git` |
| Runtime images | Root `docker-compose.yml` | Docker image registries, including Chef and `chefdemo` images |

The manifests identify direct requirements and sources; lockfiles identify
resolved versions or revisions, including transitive libraries. Those files
are the repository's detailed dependency inventories.

### Erlang / Rebar

The Erlang services require Erlang/OTP; for example, the `oc_erchef`,
`oc_bifrost`, and `bookshelf` manifests require OTP 26.2.5.21. Rebar fetches
dependencies declared in their manifests, principally from GitHub. The
manifests also declare build and test dependencies.

Primary manifests and lockfiles:

- [`src/oc_erchef/rebar.config`](src/oc_erchef/rebar.config) and
  [`src/oc_erchef/rebar.lock`](src/oc_erchef/rebar.lock)
- [`src/oc_bifrost/rebar.config`](src/oc_bifrost/rebar.config) and
  [`src/oc_bifrost/rebar.lock`](src/oc_bifrost/rebar.lock)
- [`src/bookshelf/rebar.config`](src/bookshelf/rebar.config) and
  [`src/bookshelf/rebar.lock`](src/bookshelf/rebar.lock)

`oc_erchef` also has app-level manifests and lockfiles under
[`src/oc_erchef/apps/`](src/oc_erchef/apps/), including its data collector and
test dependencies. The Rebar manifest declares dependency repositories and
branches or tags; the lockfile records resolved references.

### Ruby / Bundler

Bundler installs gems from RubyGems and, where specified, Git repositories.
Committed lockfiles record resolved gem versions and Git revisions. Relevant
manifests and locks include:

- [`src/oc-id/Gemfile`](src/oc-id/Gemfile) and
  [`src/oc-id/Gemfile.lock`](src/oc-id/Gemfile.lock)
- [`src/chef-server-ctl/Gemfile`](src/chef-server-ctl/Gemfile) and
  [`src/chef-server-ctl/Gemfile.lock`](src/chef-server-ctl/Gemfile.lock)
- [`oc-chef-pedant/Gemfile`](oc-chef-pedant/Gemfile) and
  [`oc-chef-pedant/Gemfile.lock`](oc-chef-pedant/Gemfile.lock)
- [`src/oc_bifrost/Gemfile`](src/oc_bifrost/Gemfile) and
  [`src/oc_bifrost/oc-bifrost-pedant/Gemfile`](src/oc_bifrost/oc-bifrost-pedant/Gemfile)
  and its lockfile
  ([`Gemfile.lock`](src/oc_bifrost/oc-bifrost-pedant/Gemfile.lock))
- [`src/oc_erchef/Gemfile_habitat`](src/oc_erchef/Gemfile_habitat) and
  [`src/oc_erchef/Gemfile_habitat.lock`](src/oc_erchef/Gemfile_habitat.lock)
- [`src/oc_erchef/apps/chef_objects/priv/depselector_rb/`](src/oc_erchef/apps/chef_objects/priv/depselector_rb/)

Habitat plans run Bundler as part of package builds. For example,
[`src/oc-id/habitat/plan.sh`](src/oc-id/habitat/plan.sh) installs the app's
bundle.

### Habitat packages and external sources

Habitat plans declare runtime packages in `pkg_deps` and build-time packages in
`pkg_build_deps`. These include language runtimes and system libraries/tools.
For example, the `oc_erchef` plan lists Erlang, Ruby, PostgreSQL client
libraries, OpenSSL, and build tools. Check each plan for the exact requirements
of that package:

- [`src/bookshelf/habitat/plan.sh`](src/bookshelf/habitat/plan.sh)
- [`src/chef-server-ctl/habitat/plan.sh`](src/chef-server-ctl/habitat/plan.sh)
- [`src/oc-id/habitat/plan.sh`](src/oc-id/habitat/plan.sh)
- [`src/oc_bifrost/habitat/plan.sh`](src/oc_bifrost/habitat/plan.sh)
- [`src/oc_erchef/habitat/plan.sh`](src/oc_erchef/habitat/plan.sh)
- [`src/nginx/habitat/plan.sh`](src/nginx/habitat/plan.sh)
- [`src/openresty-noroot/habitat/plan.sh`](src/openresty-noroot/habitat/plan.sh)

The OpenResty plan downloads OpenResty and LPeg source archives. Other plans
refer to packages from Habitat origins such as `core` and `chef`; the top-level
[`.bldr.toml`](.bldr.toml) maps the package names to their plans, and
[`habitat_pkgs_build.sh`](habitat_pkgs_build.sh) builds the local packages.

### Omnibus and runtime services

The `omnibus/` Git submodule supplies Omnibus build configuration maintained in
[`chef/chef-server-omnibus-config`](https://github.com/chef/chef-server-omnibus-config).
It is declared in [`.gitmodules`](.gitmodules).

The root [`docker-compose.yml`](docker-compose.yml) uses prebuilt service
images, including Chef Server components, PostgreSQL, and Elasticsearch. It
shows the images and tags used by that development stack; it is not a complete
inventory of every production runtime dependency.

## How to find the complete lists

For the dependency set of a particular component:

1. Read its `rebar.config` or `Gemfile` for direct dependencies and source
   locations.
2. Read the matching lockfile for resolved versions/revisions and transitive
   dependencies.
3. Read its Habitat `plan.sh` for Habitat runtime and build packages, and note
   any upstream archives fetched by the plan.

At runtime, additional services such as PostgreSQL and Elasticsearch are
external to this repository. The README also identifies related projects that
are maintained elsewhere, including
[knife-ec-backup](https://github.com/chef/knife-ec-backup) and
[knife-opc](https://github.com/chef/knife-opc); these are not vendored here.
