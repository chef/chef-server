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
dependency "openssl"
dependency "curl"

default_version "6.3.4"

source url: "https://github.com/Snapchat/KeyDB/archive/refs/tags/v#{version}.tar.gz"
internal_source url: "#{ENV["ARTIFACTORY_REPO_URL"]}/#{name}/#{name}-#{version}.tar.gz",
                authorization: "X-JFrog-Art-Api:#{ENV["ARTIFACTORY_TOKEN"]}"
relative_path "KeyDB-#{version}"

# version_list: url=https://github.com/Snapchat/KeyDB/archive/refs/tags/ filter=*.tar.gz
version("6.3.4") { source sha256: "229190b251f921e05aff7b0d2f04b5676c198131e2abbec1e2cfb2e61215e2f3" }

build do
  env = with_standard_compiler_flags(with_embedded_path)
  env["CFLAGS"] << " -I#{install_dir}/embedded/include"
  env["LDFLAGS"] << " -L#{install_dir}/embedded/lib"

  update_config_guess

  command "make" , env: env
  command "make install", env: env
end
