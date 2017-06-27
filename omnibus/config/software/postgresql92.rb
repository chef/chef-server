#
# Copyright 2012-2017 Chef Software, Inc.
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
default_version "9.6.3"

license "PostgreSQL"
license_file "COPYRIGHT"
skip_transitive_dependency_licensing true

source url: "https://ftp.postgresql.org/pub/source/v9.6.3/postgresql-9.6.3.tar.gz",
       sha256: "df088372230b1dd21d87bb81686471508f4c42094d4f4f32b5d8e686fea69fa6"


dependency "zlib"
dependency "openssl"
dependency "libedit"
dependency "ncurses"
dependency "libossp-uuid"
dependency "config_guess"

relative_path "postgresql-9.6.3"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  update_config_guess(target: "config")

  command "./configure" \
          " --prefix=#{install_dir}/embedded/postgresql/9.2" \
          " --with-libedit-preferred" \
          " --with-openssl" \
          " --with-ossp-uuid" \
          " --with-includes=#{install_dir}/embedded/include" \
          " --with-libraries=#{install_dir}/embedded/lib", env: env

  make "world -j #{workers}", env: env
  make "install-world -j #{workers}", env: env


  # Postgres 9.2 is our "real" Postgres installation (prior versions
  # that are installed are solely to facilitate upgrades).  As a
  # result, we need to have the binaries for this version available
  # with the other binaries used by Private Chef.
  block do
    Dir.glob("#{install_dir}/embedded/postgresql/9.2/bin/*").sort.each do |bin|
      link bin, "#{install_dir}/embedded/bin/#{File.basename(bin)}"
    end
  end
end
