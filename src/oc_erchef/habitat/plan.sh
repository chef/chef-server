pkg_name=oc_erchef
pkg_origin=chef-server
pkg_license=('Apache-2.0')
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_deps=(
  core/erlang18
  core/cacerts
  core/coreutils
  core/curl
  core/openssl/1.0.2l/20171014213633
  core/gcc-libs
  core/ruby
  core/sqitch
  core/gecode
  core/bundler
  chef-server/dbdpg
)
pkg_build_deps=(core/make core/git core/gcc)
pkg_lib_dirs=(lib)
pkg_include_dirs=(include)
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

do_download() {
  return 0
}

do_verify() {
  return 0
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

  fix_interpreter ./rebar3 core/coreutils bin/env
}


do_build() {
  _bundler_dir="$(pkg_path_for bundler)"
  export REL_VERSION=$pkg_version
  export USE_SYSTEM_GECODE=1
  export GEM_HOME="${pkg_path}/vendor/bundle"
  export GEM_PATH="${_bundler_dir}:${GEM_HOME}"
  export LIBRARY_PATH="$(pkg_path_for core/gecode)/lib"
  export LD_LIBRARY_PATH="$(pkg_path_for core/gecode)/lib"
  export CPLUS_INCLUDE_PATH="$(pkg_path_for core/gecode)/include"
  make omnibus
}

do_install() {
  export HOME="${pkg_prefix}"
  cp Gemfile_habitat ${pkg_prefix}/Gemfile
  cp Gemfile_habitat.lock ${pkg_prefix}/Gemfile.lock
  bundle install --gemfile ${pkg_prefix}/Gemfile --path "${pkg_prefix}/vendor/bundle" && bundle config path ${pkg_prefix}/vendor/bundle
  cp -r "_build/default/rel/oc_erchef/"* "${pkg_prefix}"
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
