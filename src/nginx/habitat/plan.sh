pkg_name=chef-server-nginx
pkg_origin=chef.io
pkg_version="0.1.0"
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('Apache-2.0')

pkg_deps=(
  core/curl
  core/libossp-uuid
  tcate/openresty-noroot
)
pkg_exposes=(port ssl-port)
pkg_exports=(
    [port]=port
    [ssl-port]=ssl_port
)
pkg_svc_user="hab"
pkg_svc_group="$pkg_svc_user"
pkg_binds_optional=(
  [bookshelf]="port"
  [oc_erchef]="port"
  [oc_bifrost]="port"
  [elasticsearch]="http-port"
)
pkg_description="NGINX configuration and content for Chef Server"
pkg_upstream_url="https://docs.chef.io/server_components.html"

do_build() {
  return 0
}

do_unpack() {
    mkdir -p "$HAB_CACHE_SRC_PATH/$pkg_dirname"
    cp -R "$PLAN_CONTEXT/../"* "$HAB_CACHE_SRC_PATH/$pkg_dirname"
    mkdir -p "$HAB_CACHE_SRC_PATH/$pkg_dirname/static"
    cp -R "$PLAN_CONTEXT/../../../omnibus/files/private-chef-cookbooks/private-chef/files/default/html" "$HAB_CACHE_SRC_PATH/$pkg_dirname/static"
    printenv
    echo $PLAN_CONTEXT $HAB_CACHE_SRC_PATH
#    attach
}

do_install() {
    echo PKG_SVC_STATIC_PATH $pkg_svc_static_path
    mkdir -p $pkg_svc_static_path
    cp -R $HAB_CACHE_SRC_PATH/$pkg_dirname/static "$pkg_prefix"
#    attach
    return 0
}

do_strip() {
  return 0
}


## NOT RIGHT
do_after() {
    return 0
}
