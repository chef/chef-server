pkg_name=openresty-noroot
pkg_origin=chef
pkg_version=1.27.1.1
pkg_description="Scalable Web Platform by Extending NGINX with Lua"
pkg_maintainer="The Chef Server Maintainers <support@chef.io>"
pkg_license=('BSD-2-Clause')
pkg_source=https://openresty.org/download/openresty-${pkg_version}.tar.gz
pkg_dirname=openresty-${pkg_version}
pkg_filename=openresty-${pkg_version}.tar.gz
pkg_upstream_url=http://openresty.org/
pkg_shasum=79b071e27bdc143d5f401d0dbf504de4420070d867538c5edc2546d0351fd5c0
pkg_deps=(
  core/bzip2/1.0.8/20241017112023
  core/coreutils/9.4/20241017111752
  core/gcc-libs/12.2.0/20241017104024
  core/glibc/2.36/20241017094131
  core/libxml2
  core/libxslt
  core/openssl/3.0.9/20241017121505
  core/pcre2
  core/perl
  core/zlib/1.3.1/20241017112207
)
pkg_build_deps=(
  core/gcc
  core/make
  core/which
)
pkg_lib_dirs=(lib)
pkg_bin_dirs=(bin nginx/sbin luajit/bin)
pkg_include_dirs=(include)
pkg_svc_user="hab"
pkg_exports=(
  [port]=http.listen.port
)
pkg_exposes=(port)

lpeg_version="1.0.1"
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
  wget $lpeg_source --no-check-certificate
  tar --no-same-owner -xzf lpeg-${lpeg_version}.tar.gz
  cd lpeg-${lpeg_version}
  make "LUADIR=$pkg_prefix/luajit/include/luajit-2.1" || attach
  install -p -m 0755 lpeg.so $pkg_prefix/luajit/lib/lua/5.1/ || attach
}

do_end() {
  # Clean up the `env` link, if we set it up.
  if [[ -n "$_clean_env" ]]; then
    rm -fv /usr/bin/env
  fi
}
