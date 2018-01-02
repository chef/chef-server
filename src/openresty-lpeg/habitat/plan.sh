pkg_name=openresty-lpeg
pkg_origin=irvingpop
pkg_version="0.12"
pkg_maintainer="The Habitat Maintainers <humans@habitat.sh>"
pkg_license=('MIT')
pkg_source="http://www.inf.puc-rio.br/~roberto/lpeg/lpeg-${pkg_version}.tar.gz"
pkg_shasum="efa545144cd219eee823af7624d90f78c2230677ba740b7151c5d0c303778b76"
pkg_deps=(core/openresty)
pkg_build_deps=(core/make core/gcc)
pkg_description="Parsing Expression Grammars For Lua"
pkg_upstream_url="http://www.inf.puc-rio.br/~roberto/lpeg/"
pkg_dirname="lpeg-${pkg_version}"
# pkg_filename="${pkg_name}-${pkg_version}.tar.gz"
# pkg_lib_dirs=(lib)
# pkg_include_dirs=(include)
# pkg_bin_dirs=(bin)
# pkg_pconfig_dirs=(lib/pconfig)
# pkg_svc_group="$pkg_svc_user"

do_build() {
  cd $HAB_CACHE_SRC_PATH/$pkg_dirname
  make "LUADIR=$(hab pkg path "core/openresty")/luajit/include/luajit-2.1"
}

do_install() {
  # mkdir $pkg_prefix/lib
  install -p -m 0755 lpeg.so $pkg_prefix
}
