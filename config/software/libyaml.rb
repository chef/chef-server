#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
# License:: Apache License, Version 2.0
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

name "libyaml"
version "0.1.4"

dependencies []

source :url => "http://pyyaml.org/download/libyaml/yaml-0.1.4.tar.gz",
       :md5 => "36c852831d02cf90508c29852361d01b"

relative_path "yaml-0.1.4"

configure_env =
  case platform
  when "mac_os_x"
    {
      "LDFLAGS" => "-R#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "CFLAGS" => "-I#{install_dir}/embedded/include -L#{install_dir}/embedded/lib"
    }
  when "solaris2"
    if Omnibus.config.solaris_compiler == "studio"
    {
      "LDFLAGS" => "-R#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -static-libgcc",
      "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include -DNO_VIZ"
    }
    elsif Omnibus.config.solaris_compiler == "gcc"
    {
      "LDFLAGS" => "-R#{install_dir}/embedded/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "CFLAGS" => "-I#{install_dir}/embedded/include -L#{install_dir}/embedded/lib -DNO_VIZ"
    }
    else
      raise "Sorry, #{Omnibus.config.solaris_compiler} is not a valid compiler selection."
    end
  else
    {
      "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
      "CFLAGS" => "-I#{install_dir}/embedded/include -L#{install_dir}/embedded/lib"
    }
  end

build do
  command "./configure --prefix=#{install_dir}/embedded", :env => configure_env
  command "make -j #{max_build_jobs}"
  command "make install"
end
