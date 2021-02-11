#
# Copyright 2017 Chef Software, Inc.
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

name "postgresql96"

default_version "9.6.15"

license "PostgreSQL"
license_file "COPYRIGHT"
skip_transitive_dependency_licensing true

dependency "zlib"
dependency "openssl"
dependency "libedit"
dependency "ncurses"
dependency "libossp-uuid"
dependency "config_guess"

source url: "https://ftp.postgresql.org/pub/source/v#{version}/postgresql-#{version}.tar.bz2"
version("9.6.10") { source sha256: "8615acc56646401f0ede97a767dfd27ce07a8ae9c952afdb57163b7234fe8426" }
version("9.6.14") { source sha256: "3f08c265c9ae814f727461408ab24fdf3d954c4f7ae42d9c97b3c7e03fc31a22" }
version("9.6.15") { source sha256: "3cd9fe9af247167f863030842c1a57f58bdf3e5d50a94997d34a802b6032170a" }

relative_path "postgresql-#{version}"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  short_version = version.gsub(/^([0-9]+).([0-9]+).[0-9]+$/, '\1.\2')

  update_config_guess(target: "config")

  command "./configure" \
          " --prefix=#{install_dir}/embedded/postgresql/#{short_version}" \
          " --with-libedit-preferred" \
          " --with-openssl" \
          " --with-ossp-uuid" \
          " --with-includes=#{install_dir}/embedded/include" \
          " --with-libraries=#{install_dir}/embedded/lib", env: env

  make "world -j #{workers}", env: env
  make "install-world -j #{workers}", env: env

  block do
    Dir.glob("#{install_dir}/embedded/postgresql/#{short_version}/bin/*").sort.each do |bin|
      link bin, "#{install_dir}/embedded/bin/#{File.basename(bin)}"
    end
  end
end
