pkg_name=oc_id
pkg_origin=chef-server
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('Apache-2.0')
pkg_deps=(
  core/sqitch
  core/curl
  core/node
  core/bundler
  core/ruby
  core/rsync
  core/sed
  core/libffi
  core/sqlite
  chef-server/dbdpg
  core/tzdata
)
pkg_build_deps=(
  core/git
  core/make
  core/gcc
  core/tar
  core/pkg-config
  core/coreutils
)
pkg_binds_optional=(
  [database]="port"
  [chef-server-ctl]="secrets"
)
pkg_exports=(
  [port]="port"
)
pkg_exposes=(port)
pkg_bin_dirs=(binstubs)

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
  # clean up any lingering bundle artifacts
  rm -rf $PLAN_CONTEXT/../.bundle
}

do_build() {
  export LD_LIBRARY_PATH="$(pkg_path_for core/libffi)/lib:$(pkg_path_for core/sqlite)/lib"
  export USE_SYSTEM_LIBFFI=1
  export C_INCLUDE_PATH="$(pkg_path_for core/sqlite)/include"
  export BUNDLE_SILENCE_ROOT_WARNING=1
}

_tar_pipe_app_cp_to() {
  local dst_path tar
  dst_path="$1"
  tar="$(pkg_path_for tar)/bin/tar"

  "$tar" -cp \
      --owner=root:0 \
      --group=root:0 \
      --no-xattrs \
      --exclude-backups \
      --exclude-vcs \
      --exclude='habitat' \
      --exclude='vendor/bundle' \
      --exclude='results' \
      --files-from=- \
      -f - \
  | "$tar" -x \
      -C "$dst_path" \
      -f -
}

do_install() {
  _bundler_dir="$(pkg_path_for bundler)"
  export HOME="${pkg_prefix}/oc_id"
  mkdir $HOME
  { git ls-files; git ls-files --exclude-standard --others; } \
      | _tar_pipe_app_cp_to "$HOME"
  bundle config path ${HOME}/vendor/bundle
  bundle config build.sqlite3 --with-sqlite3-lib=$(pkg_path_for core/sqlite)/lib
  bundle install --path "${HOME}/vendor/bundle" --binstubs="${HOME}/bin" --shebang ruby --deployment
  # fix tzdata location
  echo "Adding core/tzdata zoneinfo search path to tzinfo gem"
  grep -l DEFAULT_SEARCH_PATH $HOME/vendor/bundle/ruby/*/gems/tzinfo*/lib/tzinfo/zoneinfo_data_source.rb | while read -r f; do
    sed -e "s,/etc/zoneinfo,$(pkg_path_for core/tzdata)/share/zoneinfo,g" -i "$f"
  done
}

# needed due to libffi Bad value error
do_strip() {
  return 0
}
