pkg_name=chef-server-ctl
pkg_origin=chef-server
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
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
  core/postgresql
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
  return 0
}

do_build() {
  return 0
}

do_install() {
  # install gem dependencies for service hooks directly under $pkg_prefix
  export HOME="${pkg_prefix}"
  export RUBY_VENDOR="${pkg_prefix}/vendor/bundle"
  bundle install --path ${RUBY_VENDOR} --binstubs
  bundle config path ${RUBY_VENDOR}

  cp Gemfile* ${pkg_prefix}

  # install oc-chef-pedant in its own directory under $pkg_prefix
  export pedant_src_dir=$(abspath $PLAN_CONTEXT/../../../oc-chef-pedant)
  if [ ! "${pedant_src_dir}" ]; then
    exit_with "Cannot find oc-chef-pedant src directory. You must run \"hab studio enter\" from the chef-server project root." 56
  fi
  cp -pr ${pedant_src_dir} ${pkg_prefix}
  export pedant_dir="${pkg_prefix}/oc-chef-pedant"

  # TODO: declare chef gem dependency in oc-chef-pedant
  cp Gemfile.local "${pedant_dir}/Gemfile.local"

  # in pedant dir bundle install
  pushd ${pedant_dir}
  bundle install --path ${RUBY_VENDOR}
  bundle config path ${RUBY_VENDOR}
  popd

  # in chef dir bundle install
  export chef_dir="${pkg_prefix}"/chef
  mkdir $chef_dir
  pushd $chef_dir

  cat > Gemfile << EOF
source 'https://rubygems.org'
gem 'chef'
gem 'knife-opc'
EOF

  bundle install --path ${RUBY_VENDOR} --binstubs
  bundle config path ${RUBY_VENDOR}

  cp $PLAN_CONTEXT/bin/oc-chef-pedant.sh $pkg_prefix/bin/chef-server-test
  chmod +x $pkg_prefix/bin/chef-server-test

  cp $PLAN_CONTEXT/bin/knife-pivotal.sh $pkg_prefix/bin/knife
  chmod +x $pkg_prefix/bin/knife

  popd

  #
  # Chef-server-ctl install
  echo "====== BUILDING CHEF_SERVER_CTL ==== "
  echo $PLAN_CONTEXT $pkg_prefix
  export omnibus_ctl_dir="${pkg_prefix}"/omnibus-ctl

  cp -R ../../omnibus/files/private-chef-ctl-commands $omnibus_ctl_dir
  install $PLAN_CONTEXT/bin/chef-server-ctl.sh $pkg_prefix/bin/chef-server-ctl
  fix_interpreter $pkg_prefix/omnibus-ctl/chef-server-ctl core/ruby bin/ruby

  pushd ${omnibus_ctl_dir}
  bundle install --path ${RUBY_VENDOR} --binstubs
  bundle config path ${RUBY_VENDOR}
  popd

  # We install a bunch of stuff (specifically chef in private-chef-ctl) from git
  find ${RUBY_VENDOR} -name .git | xargs rm -rf
  rm -rf "${HOME}/.bundle/cache"
  rm -rf ${RUBY_VENDOR}/ruby/*/cache

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
