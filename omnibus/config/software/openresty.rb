#
# Copyright 2012-2016 Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

name "openresty"
license "BSD-2-Clause"
license_file "README.markdown"
skip_transitive_dependency_licensing true
default_version "1.13.6.1"

dependency "pcre"
dependency "openssl"
dependency "zlib"
dependency "luajit-fork" if ppc64? || ppc64le? || s390x?

source_package_name = "openresty"

version("1.13.6.1") { source sha256: "d1246e6cfa81098eea56fb88693e980d3e6b8752afae686fab271519b81d696b" }
source url: "https://openresty.org/download/#{source_package_name}-#{version}.tar.gz"

relative_path "#{source_package_name}-#{version}"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  env["PATH"] += "#{env['PATH']}:/usr/sbin:/sbin"

  if version == "1.7.10.1" && (ppc64? || ppc64le? || s390x?)
    patch source: "v1.7.10.1.ppc64le-configure.patch", plevel: 1
  end

  configure = [
    "./configure",
    "--prefix=#{install_dir}/embedded",
    "--sbin-path=#{install_dir}/embedded/sbin/nginx",
    "--conf-path=#{install_dir}/embedded/conf/nginx.conf",
    "--with-http_ssl_module",
    "--with-debug",
    "--with-http_stub_status_module",
    # Building Nginx with non-system OpenSSL
    # http://www.ruby-forum.com/topic/207287#902308
    "--with-ld-opt=\"-L#{install_dir}/embedded/lib -Wl,-rpath,#{install_dir}/embedded/lib -lssl -lcrypto -ldl -lz\"",
    "--with-cc-opt=\"-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include\"",
    # Options inspired by the OpenResty Cookbook
    "--with-md5-asm",
    "--with-sha1-asm",
    "--with-pcre-jit",
    "--without-http_ssi_module",
    "--without-mail_smtp_module",
    "--without-mail_imap_module",
    "--without-mail_pop3_module",
    "--with-ipv6",
    # AIO support define in Openresty cookbook. Requires Kernel >= 2.6.22
    # Ubuntu 10.04 reports: 2.6.32-38-server #83-Ubuntu SMP
    # However, they require libatomic-ops-dev and libaio
    #'--with-file-aio',
    #'--with-libatomic'
  ]

  # HTTP/2 was introduced with nginx 1.9.5
  if version.satisfies?(">= 1.9.5")
    configure << "--with-http_v2_module"
  end

  # for these platforms, their specific LuaJIT forks are used
  if ppc64? || ppc64le? || s390x?
    configure << "--with-luajit=#{install_dir}/embedded"
  end

  # OpenResty 1.7 + RHEL5 Fixes:
  # According to https://github.com/openresty/ngx_openresty/issues/85, OpenResty
  # fails to compile on RHEL5 without the "--with-luajit-xcflags='-std=gnu99'" flags
  if rhel? &&
      platform_version.satisfies?("< 6.0") &&
      version.satisfies?(">= 1.7")
    configure << "--with-luajit-xcflags='-std=gnu99'"
  end

  command configure.join(" "), env: env

  make "-j #{workers}", env: env
  make "install", env: env

  touch "#{install_dir}/embedded/nginx/logs/.gitkeep"
end
