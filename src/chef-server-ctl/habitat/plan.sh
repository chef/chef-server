pkg_name=chef-server-ctl
pkg_origin=chef
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('Apache-2.0')
pkg_deps=(
  core/coreutils
  core/curl
  core/jq-static
  core/ruby3_1/3.1.7
  core/libffi
  core/postgresql14-client
  core/gcc-libs
  core/glibc
)
pkg_build_deps=(
  core/glibc
  core/git
  core/diffutils
  core/patch
  core/make
  core/gcc
  core/rsync
)
pkg_bin_dirs=(bin)
pkg_exports=(
  [secrets]=secrets
)
pkg_binds_optional=(
  [chef-server-nginx]="port ssl-port"
)
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

do_unpack() {
  return 0
}

do_setup_environment() {
  export GEM_HOME="${pkg_prefix}/vendor/bundle/ruby/3.1.0"
  build_line "Setting GEM_HOME='$GEM_HOME'"
  export GEM_PATH="$GEM_HOME"
  build_line "Setting GEM_PATH='$GEM_PATH'"
}

do_build() {
  return 0
}

do_install() {
  export HOME="${pkg_prefix}"
  export RUBY_VENDOR="$pkg_prefix/vendor/bundle/ruby/3.1.0"
  mkdir -p "$RUBY_VENDOR"

  export GEM_HOME="$RUBY_VENDOR"

  # install oc-chef-pedant in its own directory under $pkg_prefix
  echo "====== BUILDING OC-CHEF-PEDANT ==== "
  export pedant_src_dir=$(abspath $PLAN_CONTEXT/../../../oc-chef-pedant)
  if [ ! "${pedant_src_dir}" ]; then
    exit_with "Cannot find oc-chef-pedant src directory. You must run \"hab studio enter\" from the chef-server project root." 56
  fi
  cp -pr ${pedant_src_dir} ${pkg_prefix}
  export pedant_dir="${pkg_prefix}/oc-chef-pedant"

  # in pedant dir bundle install
  pushd ${pedant_dir}
  bundle update
  bundle install --path ${RUBY_VENDOR}
  bundle config path ${RUBY_VENDOR}
  popd

  # in chef dir bundle install  
  echo "====== BUILDING CHEF DEPENDENCIES ==== "
  export chef_dir="${pkg_prefix}"/chef
  mkdir $chef_dir
  pushd $chef_dir

  cat > Gemfile << EOF
source 'https://rubygems.org'
gem 'chef', '~> 18.8.46'
gem 'knife', '~> 18.8.13'
EOF

  bundle install --path ${RUBY_VENDOR} --binstubs
  bundle config path ${RUBY_VENDOR}
  popd

  # Chef-server-ctl install
  echo "====== BUILDING CHEF_SERVER_CTL ==== "
  omnibus_ctl_dir="$pkg_prefix/omnibus-ctl"
  mkdir -p $omnibus_ctl_dir
  rsync -a --exclude habitat --exclude results $PLAN_CONTEXT/../ $omnibus_ctl_dir
  pushd $omnibus_ctl_dir
  bundle config path ${RUBY_VENDOR}
  bundle config binstubs binstubs
  bundle install --path ${RUBY_VENDOR} --binstubs=binstubs
  popd

  # Cleanup
  rm -rf "${HOME}/.bundle/cache"
  rm -rf ${RUBY_VENDOR}/ruby/*/cache

  # Install wrapper binaries
  wrapper_bin_path="${pkg_prefix}/bin"
  install $PLAN_CONTEXT/bin/oc-chef-pedant.sh $wrapper_bin_path/chef-server-test
  install $PLAN_CONTEXT/bin/knife-pivotal.sh $wrapper_bin_path/knife
  install $PLAN_CONTEXT/bin/chef-server-ctl.sh $wrapper_bin_path/chef-server-ctl

  for i in chef-server-test knife chef-server-ctl; do
      sed -i "s#__PKG_PATH__#${pkg_prefix}#" $wrapper_bin_path/$i
      sed -i "s#__RUBY_PATH__#$(pkg_path_for core/ruby3_1)#" $wrapper_bin_path/$i
  done
}

do_check() {
  return 0
}
