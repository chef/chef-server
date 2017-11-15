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
}

do_prepare() {
  return 0
}

do_build() {
  return 0
}

do_install() {
  # install gem dependencies for service hooks directly under $pkg_prefix
  export HOME="${pkg_prefix}"
  bundle install --path "${pkg_prefix}/vendor/bundle" --binstubs && bundle config path ${pkg_prefix}/vendor/bundle
  cp Gemfile* ${pkg_prefix}

  # install oc-chef-pedant in its own directory under $pkg_prefix
  export pedant_src_dir=$(abspath $PLAN_CONTEXT/../../../oc-chef-pedant)
  if [ ! "${pedant_src_dir}" ]; then
    exit_with "Cannot find oc-chef-pedant src directory. You must run \"hab studio enter\" from the chef-server project root." 56
  fi
  cp -pr ${pedant_src_dir} ${pkg_prefix}
  export pedant_dir="${pkg_prefix}/oc-chef-pedant"
  export HOME="${pedant_dir}"
  # TODO: declare chef gem dependency in oc-chef-pedant
  cp Gemfile.local "${pedant_dir}/Gemfile.local"
  pushd ${pedant_dir}
  bundle install --path "${pedant_dir}/vendor/bundle"
  bundle config path "${pedant_dir}/vendor/bundle"
  popd

  export HOME="${pkg_prefix}"/chef
  mkdir $HOME
  pushd $HOME

  cat > Gemfile << EOF
source 'https://rubygems.org'
gem 'chef'
gem 'knife-opc'
EOF
  
  bundle install --path "${HOME}/vendor/bundle" --binstubs && bundle config path ${HOME}/vendor/bundle || attach 

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
