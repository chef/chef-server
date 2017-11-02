pkg_name=chef-server-nginx
pkg_origin=chef.io
pkg_version="0.1.0"
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('Apache-2.0')

pkg_deps=(
  core/libossp-uuid
  core/openresty
  core/openssl
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
  mkdir -pv "$pkg_prefix/www/viz" "$pkg_prefix/www/workflow" "$pkg_prefix/www/loading"
  cp -Rv "$(pkg_path_for visibility-web)"/dist/* "$pkg_prefix/www/viz"
  cp -Rv "$(pkg_path_for workflow-web)"/dist/* "$pkg_prefix/www/workflow"
  cp -v "$SRC_PATH/unlicensed_503.html" "$pkg_prefix/www"
  cp -v "$SRC_PATH/maint_503.html" "$pkg_prefix/www"
  cp -v "$SRC_PATH/loading.html" "$pkg_prefix/www/loading/index.html"
}

do_strip() {
  return 0
}


## NOT RIGHT
do_after() {
  # Replace the license ID in the default JSON file for telemetry
  tar --extract --file="$SRC_PATH/../lunchtime-poc/delivery.license" \
    --directory="$CACHE_PATH"
  local license_id
  license_id="$(jq --raw-output .guid "$CACHE_PATH/delivery-license")"
  sed -i -e "s^LICENSE_ID^$license_id^" "$pkg_prefix/config/telemetry.default.json"
}
