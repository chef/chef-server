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

name "postgresql13"

default_version "13.22"

license "PostgreSQL"
license_file "COPYRIGHT"
skip_transitive_dependency_licensing true

dependency "zlib"
dependency "openssl"
dependency "libedit"
dependency "ncurses"
dependency "libossp-uuid"
dependency "config_guess"

# version_list: url=https://ftp.postgresql.org/pub/source/v#{version}/ filter=*.tar.bz2

version("13.22") do
  source url: "#{ENV['ARTIFACTORY_REPO_URL']}/postgresql13/#{name}-#{version}.tuxcare.1.0.0.tar.bz2",
         authorization: "X-JFrog-Art-Api:#{ENV['ARTIFACTORY_TOKEN']}",
         sha256: "d36d83dc89e625502cf6fb1d0529642ba1266bd614b4e4a41cefd1dddcf09080"
end

version("13.21") { source sha256: "dcda1294df45f033b0656cf7a8e4afbbc624c25e1b144aec79530f74d7ef4ab4" }
version("13.18") { source sha256: "ceea92abee2a8c19408d278b68de6a78b6bd3dbb4fa2d653fa7ca745d666aab1" }
version("13.14") { source sha256: "b8df078551898960bd500dc5d38a177e9905376df81fe7f2b660a1407fa6a5ed" }
version("13.5") { source sha256: "9b81067a55edbaabc418aacef457dd8477642827499560b00615a6ea6c13f6b3" }
version("13.6") { source sha256: "bafc7fa3d9d4da8fe71b84c63ba8bdfe8092935c30c0aa85c24b2c08508f67fc" }

source url: "https://ftp.postgresql.org/pub/source/v#{version}/postgresql-#{version}.tar.bz2"
internal_source url: "#{ENV["ARTIFACTORY_REPO_URL"]}/postgresql13/#{name}-#{version}.tuxcare.1.0.0.tar.bz2",
                authorization: "X-JFrog-Art-Api:#{ENV["ARTIFACTORY_TOKEN"]}"

relative_path "postgresql-#{version}"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  short_version = version.gsub(/^([0-9]+).[0-9]+$/, '\1')

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

    delete "#{install_dir}/embedded/postgresql/#{short_version}/share/doc"
  end
end
