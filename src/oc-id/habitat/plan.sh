pkg_name=oc_id
pkg_origin=chef
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('Apache-2.0')
pkg_deps=(
  core/sqitch
  core/postgresql14-client
  core/curl
  core/node
  core/ruby3_1/3.1.7
  core/rsync
  core/sed
  core/libffi
  core/sqlite
  core/tzdata
  core/openssl
  core/gcc-libs
  core/glibc
)
pkg_build_deps=(
  core/git
  core/make
  core/gcc
  core/tar
  core/pkg-config
  core/coreutils
  core/libxml2
  core/libxslt
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

do_setup_environment() {
  export GEM_HOME="${pkg_prefix}/vendor/bundle/ruby/3.1.0"
  build_line "Setting GEM_HOME='$GEM_HOME'"
  export GEM_PATH="$GEM_HOME"
  build_line "Setting GEM_PATH='$GEM_PATH'"
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
      --no-xattrs \
      --exclude-backups \
      --exclude-vcs \
      --exclude='habitat' \
      --exclude='vendor/bundle' \
      --exclude='results' \
      --files-from=- \
      -f - \
  | "$tar" -x \
      --no-same-owner \
      -C "$dst_path" \
      -f -
}

do_install() {
  export HOME="${pkg_prefix}/oc_id"
  mkdir $HOME

  export GEM_HOME="${pkg_prefix}/vendor/bundle/ruby/3.1.0"
  mkdir -p "$GEM_HOME"

  # Copy all files except excluded directories using find instead of git
  find . -type f \
    ! -path "./habitat/*" \
    ! -path "./vendor/bundle/*" \
    ! -path "./results/*" \
    ! -path "./.git/*" \
    ! -path "./.bundle/*" \
    ! -name "*.hart" \
    | _tar_pipe_app_cp_to "$HOME"
  bundle config path ${HOME}/vendor/bundle
  bundle config build.sqlite3 --with-sqlite3-lib=$(pkg_path_for core/sqlite)/lib
  bundle config build.nokogiri --with-xml2-include=$(pkg_path_for core/libxml2)/include/libxml2 \
    --with-xml2-lib=$(pkg_path_for core/libxml2)/lib \
    --with-xslt-include=$(pkg_path_for core/libxslt)/include/libxslt \
    --with-xslt-lib=$(pkg_path_for core/libxslt)/lib
  bundle config set path "${HOME}/vendor/bundle"
  bundle config set deployment 'true'
  bundle config set --local shebang 'ruby'
  bundle install --binstubs="${HOME}/bin" 
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