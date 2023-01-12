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
default_version "1.3.7"

dependency "server-open-jre"

license "Apache-2.0"
license_file "LICENSE.txt"
skip_transitive_dependency_licensing true

relative_path "opensearch-#{version}"

version "1.1.0" do
  source url: "https://artifacts.opensearch.org/releases/bundle/opensearch/#{version}/opensearch-#{version}-linux-x64.tar.gz",
         sha512: "4c2626ee56b4cdc4c8110931c369a8b9bd2c7268f5c0baa4022276e9198cf25eb7103ea3aa427a931508a4047a2a0e25fda20a2a5b1a0f6686c187b62134037a"
end

version "1.2.3" do
  source url: "https://artifacts.opensearch.org/releases/bundle/opensearch/#{version}/opensearch-#{version}-linux-x64.tar.gz",
         sha256: "a594ac2808e6e476d647e47e45e837fba3e21671285ce39b2cee1c32d5e6887d"
end

version "1.2.4" do
  source url: "https://artifacts.opensearch.org/releases/bundle/opensearch/#{version}/opensearch-#{version}-linux-x64.tar.gz",
         sha256: "d40f2696623b6766aa235997e2847a6c661a226815d4ba173292a219754bd8a8"
end

version "1.3.2" do
  source url: "https://artifacts.opensearch.org/releases/bundle/opensearch/#{version}/opensearch-#{version}-linux-x64.tar.gz",
         sha256: "14199251a8aae2068fd54aa39c778ff29dcc8be33d57f36a8cc2d19e07ff4149"
end

version "1.3.6" do
  source url: "https://artifacts.opensearch.org/releases/bundle/opensearch/#{version}/opensearch-#{version}-linux-x64.tar.gz",
         sha256: "0784cc05ec03dc9cac17dca923272ae08ebc9a43fbbbb61397024f1c90cdb024"
end

version "1.3.7" do
  source url: "https://artifacts.opensearch.org/releases/bundle/opensearch/#{version}/opensearch-#{version}-linux-x64.tar.gz",
         sha256: "541a371f71d6df7bfb643832c8c1291180d082918623987de00b67d0c560a8fa"
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
  command "cd #{project_dir}/plugins; ls | grep -v opensearch-security| xargs rm -rf "
  # by default RPMs will not include empty directories in the final package
  # OpenSearch will fail to start if this dir is not present.
  # touch  "#{project_dir}/plugins/.gitkeep"

  sync   "#{project_dir}/", "#{target_path}"
  command "chmod ugo+x #{project_dir}/plugins/opensearch-security/tools/*"
  command "chmod ugo+x #{target_path}/plugins/opensearch-security/tools/*"
  # Dropping a VERSION file here allows additional software definitions
  # to read it to determine OpenSearch plugin compatibility.
  command "echo #{version} > #{target_path}/VERSION"
end
