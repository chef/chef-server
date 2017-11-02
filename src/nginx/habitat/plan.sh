pkg_name=chef-server-nginx
pkg_origin=chef.io
pkg_version="0.1.0"
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('Apache-2.0')

pkg_deps=(
  core/libossp-uuid
  core/openresty
)
# pkg_build_deps=(
#   $HAB_ORIGIN/visibility-web
#   $HAB_ORIGIN/workflow-web
# )
pkg_expose=(443 80)
pkg_svc_user="root"
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

# NOT RIGHT
do_install() {
    return 0
}

do_strip() {
  return 0
}


## NOT RIGHT
do_after() {
    return 0
}
