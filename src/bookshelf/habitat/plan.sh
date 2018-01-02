pkg_name=bookshelf
pkg_origin=chef-server
pkg_license=('Apache-2.0')
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_source="nosuchfile.tar.gz"
pkg_deps=(
  core/erlang18
  core/cacerts
  core/coreutils
  core/gcc-libs
  core/sqitch
  chef-server/dbdpg
)
pkg_build_deps=(core/make core/git core/gcc)
pkg_lib_dirs=(lib)
pkg_include_dirs=(include)
pkg_bin_dirs=(bin)
pkg_description="Bookshelf is an S3 API compatible object store."
pkg_upstream_url="https://github.com/chef/chef-server"
pkg_exports=(
  [port]="port"
)
pkg_exposes=(port)

pkg_binds_optional=(
  [database]="port"
  [chef-server-ctl]="secrets"
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
  export REL_VERSION=$pkg_version
  make omnibus
}

do_install() {
  cp -rv "_build/default/rel/bookshelf/"* "${pkg_prefix}"
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
