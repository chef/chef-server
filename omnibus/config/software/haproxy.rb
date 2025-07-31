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
name "haproxy"
default_version "3.0.11"

dependency "zlib"
dependency "pcre"
dependency "openssl"

license "GPL-2.0"
license_file "LICENSE"
skip_transitive_dependency_licensing true

# HTTPS is available but certificate validation fails on OS X
source url: "http://www.haproxy.org/download/3.0/src/haproxy-#{version}.tar.gz",
       sha256: "a133e2d550c5fd9a849b5c7ab17bb945bcdad209ca140d41f45ebf31943ae783"

relative_path "haproxy-#{version}"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  #
  # Many of these are the same environment variables that debian sets
  # PREFIX and TARGET are mandatory to get it building under omnibus.
  #
  build_options = {
   "PREFIX" => "#{install_dir}/embedded",
   # Use libpcre for regex, libpcre > 8.32 required
   # for JIT
   "USE_PCRE" => "1",
   "USE_PCRE_JIT" => "1",
   "USE_ZLIB" => "1",
   "USE_OPENSSL" => "1",
   # Required to resolve hostnames to IPv6 addresses
   # off-by-default because of prolems on older glibc's
   "USE_GETADDRINFO" => "1",
   "TARGET" => "linux2628"
  }

  if intel?
    build_options["USE_REGPARM"] = "1"
  end

  build_args = ""
  build_options.each { |k,v| build_args << " #{k}=#{v}"}
  make "#{build_args}", env: env
  make "install #{build_args}", env: env
end
