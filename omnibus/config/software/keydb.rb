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

name "keydb"

license "BSD-3-Clause"
license_file "COPYING"
skip_transitive_dependency_licensing true

dependency "config_guess"
default_version "6.2.2"

version("6.2.2") { source sha256: "368da41404b414a5baea78ad0110ecee5fda779758c7b0762ac788549ac24db9" }

# source url: "https://download.keydb.dev/pkg/open_source/deb/ubuntu18.04_bionic/amd64/keydb_all_versions/keydb-6.3.4/keydb-server_6.3.4-1~bionic1_amd64.deb"

source url: "https://github.com/Snapchat/KeyDB/archive/refs/tags/v6.2.2.zip"


relative_path "KeyDB-#{version}"

build do
  env = with_standard_compiler_flags(with_embedded_path).merge(
    "PREFIX" => "#{install_dir}/embedded"
  )

  update_config_guess

  if version.satisfies?("< 6.0")
    patch source: "password-from-environment.patch", plevel: 1, env: env
  end

  make "-j #{workers}", env: env
  make "install", env: env
end
