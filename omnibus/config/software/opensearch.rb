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

name "opensearch"
default_version "1.1.0"

dependency "server-open-jre"

license "Apache-2.0"
license_file "LICENSE.txt"
skip_transitive_dependency_licensing true

relative_path "opensearch-#{version}"

version "1.1.0" do
  source url: "https://artifacts.opensearch.org/releases/bundle/opensearch/#{version}/opensearch-#{version}-linux-x64.tar.gz",
         sha512: "4c2626ee56b4cdc4c8110931c369a8b9bd2c7268f5c0baa4022276e9198cf25eb7103ea3aa427a931508a4047a2a0e25fda20a2a5b1a0f6686c187b62134037a"
end

target_path = "#{install_dir}/embedded/opensearch"

build do
  mkdir  "#{target_path}"
  delete "#{project_dir}/lib/sigar/*solaris*"
  delete "#{project_dir}/lib/sigar/*sparc*"
  delete "#{project_dir}/lib/sigar/*freebsd*"
  delete "#{project_dir}/config"
  delete "#{project_dir}/jdk"
  delete "#{project_dir}/modules/x-pack-ml"
  delete "#{project_dir}/modules/ingest-geoip"
  mkdir  "#{project_dir}/plugins"
  # by default RPMs will not include empty directories in the final package
  # ES will fail to start if this dir is not present.
  touch  "#{project_dir}/plugins/.gitkeep"

  sync   "#{project_dir}/", "#{target_path}"

  # Dropping a VERSION file here allows additional software definitions
  # to read it to determine ES plugin compatibility.
  command "echo #{version} > #{target_path}/VERSION"
end
