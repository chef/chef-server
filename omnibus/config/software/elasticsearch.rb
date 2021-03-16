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

name "elasticsearch"
default_version "6.8.14"

dependency "server-open-jre"

license "Apache-2.0"
license_file "LICENSE.txt"
skip_transitive_dependency_licensing true

relative_path "elasticsearch-#{version}"

version "6.8.14" do
  source url: "https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-oss-#{version}.tar.gz",
         sha512: "c0372969ec9e5f7aff551bc0aa13684fdaccd5a553e8ee896979488ab2f13181642222ab4eca4dc799181bce1405200b5aea4866c830483ce1e484fa6a714998"
end

version "7.9.1" do
  source url: "https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-#{version}-linux-x86_64.tar.gz",
         sha512: "e24aab0fbeb0b53cc386bb0ca1fc84c457851c5d80d147324bf97ff42f063332a93dec3c693550662393a72c7a0522a100181dd9a7d50b3e487a0f2a2a9bbcc0"
end

target_path = "#{install_dir}/embedded/elasticsearch"

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
