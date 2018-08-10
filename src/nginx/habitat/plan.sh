pkg_name=chef-server-nginx
pkg_origin=chef
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('Apache-2.0')
pkg_deps=(
  core/curl
  core/libossp-uuid
  ${HAB_ORIGIN:-chef}/openresty-noroot
)
pkg_build_deps=()
pkg_exposes=(port ssl-port)
pkg_exports=(
    [port]=http.port
    [ssl-port]=https.port
)
pkg_binds_optional=(
  [bookshelf]="port"
  [chef-server-ctl]="secrets"
  [oc_erchef]="port data_collector_enabled data_collector_server data_collector_port"
  [oc_bifrost]="port"
  [elasticsearch]="http-port"
  [oc_id]="port"
)
pkg_description="NGINX configuration and content for Chef Server"
pkg_upstream_url="https://docs.chef.io/server_components.html"
pkg_svc_run="openresty -c ${pkg_svc_config_path}/nginx.conf -p ${pkg_svc_var_path}"

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
    mkdir -p "$HAB_CACHE_SRC_PATH/$pkg_dirname"
    cp -R "$PLAN_CONTEXT/../"* "$HAB_CACHE_SRC_PATH/$pkg_dirname"
    mkdir -p "$HAB_CACHE_SRC_PATH/$pkg_dirname/static"
    cp -R "$PLAN_CONTEXT/../../../omnibus/files/private-chef-cookbooks/private-chef/files/default/html" "$HAB_CACHE_SRC_PATH/$pkg_dirname/static"
}

do_build() {
  return 0
}

do_install() {
    echo PKG_SVC_STATIC_PATH $pkg_svc_static_path
    mkdir -p $pkg_svc_static_path
    cp -R $HAB_CACHE_SRC_PATH/$pkg_dirname/static "$pkg_prefix"
    return 0
}

do_strip() {
  return 0
}


## NOT RIGHT
do_after() {
    return 0
}
