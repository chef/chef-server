#
# Copyright:: Copyright (c) 2012-2013 Opscode, Inc.
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

name "postgresql92"
version "9.2.4"

dependency "zlib"
dependency "openssl"
dependency "libedit"
dependency "ncurses"

source :url => "http://ftp.postgresql.org/pub/source/v9.2.4/postgresql-9.2.4.tar.bz2",
       :md5 => "6ee5bb53b97da7c6ad9cb0825d3300dd"

relative_path "postgresql-9.2.4"

configure_env = {
  "LDFLAGS" => "-Wl,-rpath,#{install_dir}/embedded/lib,-rpath,#{install_dir}/embedded/postgresql/9.2/lib -L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command ["./configure",
           "--prefix=#{install_dir}/embedded/postgresql/9.2",
           "--with-libedit-preferred",
           "--with-openssl",
           "--with-includes=#{install_dir}/embedded/include",
           "--with-libraries=#{install_dir}/embedded/lib"].join(" "), :env => configure_env
  command "make world -j #{max_build_jobs}", :env => {"LD_RUN_PATH" => "#{install_dir}/embedded/lib"}
  command "make install-world"
end
