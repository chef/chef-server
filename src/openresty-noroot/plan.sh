pkg_name=openresty-noroot
pkg_origin=chef-server
pkg_version=1.11.2.5
pkg_description="Scalable Web Platform by Extending NGINX with Lua"
pkg_maintainer="The Habitat Maintainers <humans@habitat.sh>"
pkg_license=('BSD-2-Clause')
pkg_source=https://openresty.org/download/openresty-${pkg_version}.tar.gz
pkg_dirname=openresty-${pkg_version}
pkg_filenam=openresty-${pkg_version}.tar.gz
pkg_upstream_url=http://openresty.org/
pkg_shasum=f8cc203e8c0fcd69676f65506a3417097fc445f57820aa8e92d7888d8ad657b9
pkg_deps=(core/glibc core/gcc-libs core/libxml2 core/libxslt core/zlib core/bzip2 core/openssl/1.0.2l/20171014213633 core/pcre core/coreutils core/perl core/which)
pkg_build_deps=(core/gcc core/make)
pkg_lib_dirs=(lib)
pkg_bin_dirs=(bin nginx/sbin luajit/bin)
pkg_include_dirs=(include)
pkg_svc_user="hab"
pkg_exports=(
  [port]=http.listen.port
)
pkg_exposes=(port)

lpeg_version="0.12"
lpeg_source="http://www.inf.puc-rio.br/~roberto/lpeg/lpeg-${lpeg_version}.tar.gz"

do_prepare() {
  # The `/usr/bin/env` path is hardcoded, so we'll add a symlink.
  if [[ ! -r /usr/bin/env ]]; then
    ln -sv "$(pkg_path_for coreutils)/bin/env" /usr/bin/env
    _clean_env=true
  fi
}

do_build() {
  ./configure --prefix="$pkg_prefix" \
    --user=hab \
    --group=hab \
    --http-log-path=stdout \
    --error-log-path=stderr \
    --with-ipv6 \
    --with-debug \
    --with-pcre \
    --with-md5-asm \
    --with-pcre-jit \
    --with-sha1-asm \
    --with-file-aio \
    --with-luajit \
    --with-stream=dynamic \
    --with-mail=dynamic \
    --with-http_gunzip_module \
    --with-http_gzip_static_module \
    --with-http_realip_module \
    --with-http_v2_module \
    --with-http_ssl_module \
    --with-http_stub_status_module \
    --with-http_addition_module \
    --with-http_degradation_module \
    --with-http_flv_module \
    --with-http_mp4_module \
    --with-http_secure_link_module \
    --with-http_sub_module \
    --with-http_slice_module \
    --with-cc-opt="$CFLAGS" \
    --with-ld-opt="$LDFLAGS" \
    --without-http_ssi_module \
    --without-mail_smtp_module \
    --without-mail_imap_module \
    --without-mail_pop3_module \
    -j"$(nproc)"

  make -j"$(nproc)"

}

do_install() {
  make install
  fix_interpreter "$pkg_prefix/bin/*" core/coreutils bin/env

  cd $HAB_CACHE_SRC_PATH
  wget $lpeg_source
  tar -xzf lpeg-${lpeg_version}.tar.gz
  cd lpeg-${lpeg_version}
  make "LUADIR=$pkg_prefix/luajit/include/luajit-2.1" || attach
  install -p -m 0755 lpeg.so $pkg_prefix/luajit/lib/lua/5.1/ || attach
#  cp $(hab pkg path "chef-server/openresty-lpeg")/lpeg.so ${pkg_prefix}/luajit/lib/lua/5.1/ || attach
}

do_end() {
  # Clean up the `env` link, if we set it up.
  if [[ -n "$_clean_env" ]]; then
    rm -fv /usr/bin/env
  fi
}
