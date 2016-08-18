#
# Copyright 2016 Chef Software, Inc.
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
default_version "1.6.4"

dependency "zlib"
dependency "pcre"
dependency "openssl"

license "GPL-2.0"
license_file "LICENSE"
skip_transitive_dependency_licensing true

# HTTPS is available but certificate validation fails on OS X
source url: "http://www.haproxy.org/download/1.6/src/haproxy-#{version}.tar.gz",
       sha256: "e5fa3c604f1fe9ecb6974ccda6705c105ebee14b3a913069fb08f00e860cd230"

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
  }
  # Required to resolve hostnames to IPv6 addresses
  # off-by-default because of prolems on older glibc's
  # TODO(ssd): Should we turn this off on RHEL5?
  build_options['USE_GETADDRINFO'] = "1"
  if intel?
    build_options["USE_REGPARM"] = "1"
  end
  build_options['TARGET'] = if ohai["kernel"] && ohai["kernel"]["name"] == "Linux"
                              version = Gem::Version.new(String(ohai["kernel"]["release"]).split("-").first)
                              case
                              when version >= Gem::Version.new("2.6.28")
                                "linux2628"
                              when version >= Gem::Version.new("2.6")
                                "linux26"
                              else
                                "linux24e"
                              end
                            else
                              "generic"
                            end
  build_args = ""
  build_options.each { |k,v| build_args << " #{k}=#{v}"}
  make "haproxy #{build_args}", env: env
  make "install #{build_args}", env: env
end
