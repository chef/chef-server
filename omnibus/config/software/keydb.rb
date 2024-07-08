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
default_version "6.3.4"

version("6.3.4") { source sha256: "6aea5ddbfe4f2137c6ea933b42592862bed6ae424a130885aff82273fe9641d0" }

source url: "https://download.keydb.dev/pkg/open_source/deb/ubuntu18.04_bionic/amd64/keydb_all_versions/keydb-6.3.4/keydb-server_6.3.4-1~bionic1_amd64.deb"

relative_path "keydb-#{version}"

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
