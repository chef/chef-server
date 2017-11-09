pkg_name=chef-server-ctl
pkg_origin=chef
pkg_version="0.1.0"
pkg_maintainer="The Habitat Maintainers <humans@habitat.sh>"
pkg_license=('Apache-2.0')
# pkg_source="http://some_source_url/releases/${pkg_name}-${pkg_version}.tar.gz"
# pkg_filename="${pkg_name}-${pkg_version}.tar.gz"
# pkg_shasum="TODO"
pkg_deps=(
  core/coreutils
  core/curl
  core/jq-static
  core/ruby
  core/bundler
  core/hab-butterfly
)
pkg_build_deps=(
  core/coreutils
  core/glibc
  core/git
  core/diffutils
  core/patch
  core/make
  core/gcc
)
pkg_lib_dirs=(lib)
pkg_include_dirs=(include)
pkg_bin_dirs=(bin)
pkg_exports=(
  [secrets]=secrets
)
# pkg_exposes=(port ssl-port)
# pkg_binds=(
#   [database]="port host"
# )
pkg_interpreters=(bin/bash)
pkg_svc_user="hab"
pkg_svc_group="$pkg_svc_user"
pkg_description="Some description."

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
    chef_src_dir="$HAB_CACHE_SRC_PATH/$pkg_dirname/src/github.com/"
  mkdir -p $chef_src_dir
  pushd $chef_src_dir
  git clone https://github.com/chef/chef-server.git
  popd
}

do_prepare() {
  return 0
}

do_build() {
  # _bundler_dir="$(pkg_path_for bundler)"
  export REL_VERSION="12.99.1"
  export GEM_HOME="${pkg_path}/vendor/bundle"
  export GEM_PATH="${_bundler_dir}:${GEM_HOME}"
  return 0
}

do_install() {
  export HOME="${pkg_prefix}"
  bundle install --path "${pkg_prefix}/vendor/bundle" --binstubs && bundle config path ${pkg_prefix}/vendor/bundle
  cp Gemfile* ${pkg_prefix}
  # install oc-chef-pedant
  pedant_src_dir="$HAB_CACHE_SRC_PATH/$pkg_dirname/src/github.com/chef-server/oc-chef-pedant"
  cp -pr $pedant_src_dir ${pkg_prefix}
  cp Gemfile.local "${pkg_prefix}/oc-chef-pedant/"
  pushd "${pkg_prefix}/oc-chef-pedant"
  bundle install --path "vendor/bundle" && bundle config path ${pkg_prefix}/vendor/bundle
  #export GEM_HOME="${pkg_prefix}/vendor/bundle/ruby/2.4.0"
  #export GEM_PATH="${GEM_HOME}"
  # gem build oc-chef-pedant.gemspec && gem install oc-chef-pedant-*.gem --no-ri --no-rdoc
  popd
}

do_check() {
  return 0
}

do_end() {
  # Clean up the `env` link, if we set it up.
  if [[ -n "$_clean_env" ]]; then
    rm -fv /usr/bin/env
  fi
}
