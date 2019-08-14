#
# Copyright 2019 Chef Software, Inc.
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
default_version "6.8.1"

dependency "server-open-jre"
#dependency "openjdk"

license "Apache-2.0"
license_file "LICENSE.txt"
skip_transitive_dependency_licensing true

#TODO: Look at deleting the older versions.
source url: "https://download.elastic.co/elasticsearch/release/org/elasticsearch/distribution/tar/elasticsearch/#{version}/elasticsearch-#{version}.tar.gz"
relative_path "elasticsearch-#{version}"

version "2.3.1" do
  source sha256: "f0092e73038e0472fcdd923e5f2792e13692ea0f09ca034a54dd49b217110ebb"
end

version "2.4.1" do
  source sha256: "23a369ef42955c19aaaf9e34891eea3a055ed217d7fbe76da0998a7a54bbe167"
end

version "5.4.1" do
  # Newer versions appear to live in an alternative location that does
  # not also contain the older versions. We can make this default when we drop 2.x.
  source url: "https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-#{version}.tar.gz",
         sha256: "09d6422bd33b82f065760cd49a31f2fec504f2a5255e497c81050fd3dceec485"
end

version "5.6.16" do
  source url: "https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-#{version}.tar.gz",
         sha256: "6b035a59337d571ab70cea72cc55225c027ad142fbb07fd8984e54261657c77f"
end

version "6.8.1" do
  source url: "https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-#{version}.tar.gz",
         sha512: "1d484287e9b67b16c28f1a4d2267e7ceb5a4438a18b26b3a46d4a176bb3f2f6fcadcbda617a7a91418293880d38c027266cb81a4e8893a28adee9fa693b2318b"
end

target_path = "#{install_dir}/embedded/elasticsearch"

build do
  mkdir  "#{target_path}"
  delete "#{project_dir}/lib/sigar/*solaris*"
  delete "#{project_dir}/lib/sigar/*sparc*"
  delete "#{project_dir}/lib/sigar/*freebsd*"
  delete "#{project_dir}/config"
  delete "#{project_dir}/modules/x-pack-ml"
  mkdir  "#{project_dir}/plugins"
  # by default RPMs will not include empty directories in the final package
  # ES will fail to start if this dir is not present.
  touch  "#{project_dir}/plugins/.gitkeep"

  sync   "#{project_dir}/", "#{target_path}"

  # Dropping a VERSION file here allows additional software definitions
  # to read it to determine ES plugin compatibility.
  command "echo #{version} > #{target_path}/VERSION"
end
