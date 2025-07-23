#
# Copyright:: Chef Software, Inc.
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

name "erlang"
default_version "26.2.5.14"

license "Apache-2.0"
license_file "LICENSE.txt"
skip_transitive_dependency_licensing true

dependency "zlib"
dependency "openssl"
dependency "ncurses"
dependency "config_guess"

# grab from github so we can get patch releases if we need to
source url: "https://github.com/erlang/otp/archive/OTP-#{version}.tar.gz"
internal_source url: "#{ENV["ARTIFACTORY_REPO_URL"]}/#{name}/#{name}-#{version}.tar.gz",
                authorization: "X-JFrog-Art-Api:#{ENV["ARTIFACTORY_TOKEN"]}"
relative_path "otp-OTP-#{version}"

# versions_list: https://github.com/erlang/otp/tags filter=*.tar.gz
# to get the SHA256, download the tar.gz, then calculate the SHA256 on it
version("26.2.5.14") { source sha256: "5378dc60382c3d43ecdc0e8666c5db0f8a1df1525fff706779f720ad1d54c56c" }
version("26.2.5.2")  { source sha256: "8e537e2d984770796cc7f0c7c079a9e5fbc67b8c368e0dcd9aa2ceaeb2844da2" } 
version("26.2.2")    { source sha256: "93c09aa8814018c23d218ac68b2bcdba188e12086223fbfa08af5cc70edd7ee1" } 
version("25.2")      { source sha256: "d33a988f39e534aff67799c5b9635612858459c9d8890772546d71ea38de897a" }
version("25.1.2")    { source sha256: "b9ae7becd3499aeac9f94f9379e2b1b4dced4855454fe7f200a6e3e1cf4fbc53" }
version("25.0.4")    { source sha256: "05878cb51a64b33c86836b12a21903075c300409b609ad5e941ddb0feb8c2120" }
version("25.0.2")    { source sha256: "f78764c6fd504f7b264c47e469c0fcb86a01c92344dc9d625dfd42f6c3ed8224" }
version("25.0")      { source sha256: "5988e3bca208486494446e885ca2149fe487ee115cbc3770535fd22a795af5d2" }
version("24.3.4.7")  { source sha256: "80c08cf1c181a124dd805bb1d91ff5c1996bd8a27b3f4d008b1ababf48d9947e" }
version("24.3.4")    { source sha256: "e59bedbb871af52244ca5284fd0a572d52128abd4decf4347fe2aef047b65c58" }
version("24.3.3")    { source sha256: "a5f4d83426fd3dc2f08c0c823ae29bcf72b69008a2baee66d27ad614ec7ab607" }
version("24.3.2")    { source sha256: "cdc9cf788d28a492eb6b24881fbd06a0a5c785dc374ad415b3be1db96326583c" }
version("18.3")      { source sha256: "a6d08eb7df06e749ccaf3049b33ceae617a3c466c6a640ee8d248c2372d48f4e" }

build do
  # Don't listen on 127.0.0.1/::1 implicitly whenever ERL_EPMD_ADDRESS is given
  if version.satisfies?("<= 24.3.3")
    patch source: "epmd-require-explicitly-adding-loopback-address.patch", plevel: 1
  else
    patch source: "updated-epmd-require-explicitly-adding-loopback-address.patch", plevel: 1
  end

  env = with_standard_compiler_flags(with_embedded_path).merge(
    # WARNING!
    "CFLAGS"  => "-L#{install_dir}/embedded/lib -O3 -I#{install_dir}/embedded/erlang/include",
    "LDFLAGS" => "-Wl,-rpath #{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/erlang/include"
  )
  env.delete("CPPFLAGS")

  # The TYPE env var sets the type of emulator you want
  # We want the default so we give TYPE and empty value
  # in case it was set by CI.
  env["TYPE"] = ""

  update_config_guess(target: "erts/autoconf")
  update_config_guess(target: "lib/common_test/priv/auxdir")
  update_config_guess(target: "lib/wx/autoconf")

  if version.satisfies?(">= 19.0")
    update_config_guess(target: "lib/common_test/test_server/src")
  else
    update_config_guess(target: "lib/test_server/src")
  end

  # Setup the erlang include dir
  mkdir "#{install_dir}/embedded/erlang/include"

  # At this time, erlang does not expose a way to specify the path(s) to these
  # libraries, but it looks in its local +include+ directory as part of the
  # search, so we will symlink them here so they are picked up.
  #
  # In future releases of erlang, someone should check if these flags (or
  # environment variables) are avaiable to remove this ugly hack.
  %w{ncurses openssl zlib.h zconf.h}.each do |name|
    link "#{install_dir}/embedded/include/#{name}", "#{install_dir}/embedded/erlang/include/#{name}"
  end

  # Note 2017-02-28 sr: HiPE doesn't compile with OTP 18.3 on ppc64le (https://bugs.erlang.org/browse/ERL-369)
  # Compiling fails when linking beam.smp, with
  #     powerpc64le-linux-gnu/libutil.so: error adding symbols: File in wrong format
  #
  # We've been having issues with ppc64le and hipe before, too:
  # https://github.com/chef/chef-server/commit/4fa25ed695acaf819b11f71c6db1aab5c8adcaee
  #
  # It's trying to compile using a linker script for ppc64, it seems:
  # https://github.com/erlang/otp/blob/c1ea854fac3d8ed14/erts/emulator/hipe/elf64ppc.x
  # Probably introduced with https://github.com/erlang/otp/commit/37d63e9b8a0a96
  # See also https://sourceware.org/ml/binutils/2015-05/msg00148.html
  hipe = ppc64le? ? "disable" : "enable"

  unless File.exist?("./configure")
    # Building from github source requires this step
    command "./otp_build autoconf"
  end
  # Note: et, debugger and observer applications require wx to
  # build. The tarballs from the downloads site has prebuilt the beam
  # files, so we were able to get away without disabling them and
  # still build. When building from raw source we must disable them
  # explicitly.
  wx = "without"

  command "./configure" \
          " --prefix=#{install_dir}/embedded" \
          " --enable-threads" \
          " --enable-smp-support" \
          " --enable-kernel-poll" \
          " --enable-dynamic-ssl-lib" \
          " --enable-shared-zlib" \
          " --enable-fips" \
          " --#{hipe}-hipe" \
          " --#{wx}-wx" \
          " --#{wx}-et" \
          " --#{wx}-debugger" \
          " --#{wx}-observer" \
          " --without-megaco" \
          " --without-javac" \
          " --with-ssl=#{install_dir}/embedded" \
          " --disable-debug", env: env

  make "-j #{workers}", env: env
  make "install", env: env
end
