pkg_name=oc_erchef
pkg_origin=chef
pkg_license=('Apache-2.0')
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_deps=(
  core/erlang20
  core/cacerts
  core/coreutils
  core/curl
  core/openssl
  core/gcc-libs
  core/ruby26
  core/sqitch_pg
  core/gecode
  core/libffi
  core/glibc
)
pkg_build_deps=(core/make core/git core/gcc)
pkg_bin_dirs=(bin)
pkg_description="Erlang implementation of the Chef Server's REST API."
pkg_upstream_url="https://github.com/chef/chef-server"

pkg_exposes=(port)
pkg_exports=(
  [port]=oc_chef_wm.port
  [data_collector_enabled]=data_collector.enabled
  [data_collector_server]=data_collector.server
  [data_collector_port]=data_collector.port
)

pkg_binds_optional=(
  [chef-server-ctl]="secrets"
  [database]="port"
  [elasticsearch]="http-port"
  [oc_bifrost]="port"
  [bookshelf]="port"
)

pkg_version() {
  cat "$PLAN_CONTEXT/../../../VERSION"
}

do_before() {
  do_default_before
  if [ ! -f "$PLAN_CONTEXT/../../../VERSION" ]; then
    exit_with "Cannot find VERSION file! You must run \"hab studio enter\" from the chef-server project root." 56
  fi
  update_pkg_version
}

do_unpack() {
  # Copy everything over to the cache path so we don't write out our compiled
  # deps into the working directory, but into the cache directory.
  mkdir -p "$HAB_CACHE_SRC_PATH/$pkg_dirname"
  cp -R "$PLAN_CONTEXT/../"* "$HAB_CACHE_SRC_PATH/$pkg_dirname"
}

do_prepare() {
  # The `/usr/bin/env` path is hardcoded in jiffy, so we'll add a symlink since fix_interpreter won't work.
  if [[ ! -r /usr/bin/env ]]; then
    ln -sv "$(pkg_path_for coreutils)/bin/env" /usr/bin/env
    _clean_env=true
  fi

  # Need this for enterprise_ctl to compile with an old version of rebar
  build_line "Setting PATH=$PATH:$HAB_CACHE_SRC_PATH/$pkg_dirname"
  export PATH=$PATH:"$HAB_CACHE_SRC_PATH/$pkg_dirname"

  build_line "RELX_OUTPUT_DIR=$pkg_prefix"
  export RELX_OUTPUT_DIR=$pkg_prefix

  git config --global http.sslCAInfo \
    "$(pkg_path_for core/cacerts)"/ssl/certs/cacert.pem
}


do_build() {
  _ruby_dir="$(pkg_path_for core/ruby26)"
  export REL_VERSION=$pkg_version
  export USE_SYSTEM_GECODE=1
  export GEM_HOME="${pkg_path}/vendor/bundle"
  export GEM_PATH="${_ruby_dir}:${GEM_HOME}"
  export LIBRARY_PATH="$(pkg_path_for core/gecode)/lib"
  export LD_LIBRARY_PATH="$(pkg_path_for core/gecode)/lib"
  export CPLUS_INCLUDE_PATH="$(pkg_path_for core/gecode)/include"
  mkdir -p "$GEM_HOME"
  gem install bundler:1.17.2

  make omnibus
}

do_install() {
  export HOME="${pkg_prefix}"
  export GEM_HOME="${pkg_prefix}/vendor/bundle"

  cp Gemfile_habitat ${pkg_prefix}/Gemfile
  cp Gemfile_habitat.lock ${pkg_prefix}/Gemfile.lock
  bundle install --gemfile ${pkg_prefix}/Gemfile --path "${pkg_prefix}/vendor/bundle" && bundle config path ${pkg_prefix}/vendor/bundle
  cp -r "_build/default/rel/oc_erchef/"* "${pkg_prefix}"
  fix_interpreter "${pkg_prefix}/bin/reindex-opc-organization" core/coreutils bin/env
  cp -R "$HAB_CACHE_SRC_PATH/$pkg_dirname/schema" "$pkg_prefix"
}

do_check() {
  make
}

do_end() {
  # Clean up the `env` link, if we set it up.
  if [[ -n "$_clean_env" ]]; then
    rm -fv /usr/bin/env
  fi
}
