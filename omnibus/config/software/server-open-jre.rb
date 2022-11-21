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
dependency "zlib"
dependency "patchelf"

name "server-open-jre"
default_version "11.0.17+8"

unless _64_bit?
  raise "Server-open-jre can only be installed on x86_64 systems."
end

license "GPL-2.0 (with the Classpath Exception)"

license_file "http://openjdk.java.net/legal/gplv2+ce.html"
skip_transitive_dependency_licensing true

whitelist_file "jre/bin/javaws"
whitelist_file "jre/bin/policytool"
whitelist_file "jre/lib"
whitelist_file "jre/plugin"
whitelist_file "jre/bin/appletviewer"

license_warning = "By including the JRE, you accept the terms of AdoptOpenJRE."

# version_list: url=https://github.com/adoptium/temurin11-binaries/releases filter=*.tar.gz

version "11.0.17+8" do
  source url: "https://github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.17%2B8/OpenJDK11U-jre_x64_linux_hotspot_11.0.17_8.tar.gz",
  sha256: "752616097e09d7f60a3ad8bd312f90eaf50ac72577e55df229fe6e8091148f79",
  warning: license_warning,
  unsafe: true
end

version "11.0.16.1+1" do
  source url: "https://github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.16.1%2B1/OpenJDK11U-jre_x64_linux_hotspot_11.0.16.1_1.tar.gz",
  sha256: "1ffe1a682e8179e35238bf3f93aba0cb185850e202c676f41d38cb0561883eda",
  warning: license_warning,
  unsafe: true
end

version "11.0.15+10" do
  source url: "https://github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.15%2B10/OpenJDK11U-jre_x64_linux_hotspot_11.0.15_10.tar.gz",
  sha256: "22831fd097dfb39e844cb34f42064ff26a0ada9cd13621d7b8bca8e9b9d3a5ee",
  warning: license_warning,
  unsafe: true
end

version "11.0.14.1+1" do
  source url: "https://github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.14.1%2B1/OpenJDK11U-jre_x64_linux_hotspot_11.0.14.1_1.tar.gz",
  sha256: "b5a6960bc6bb0b1a967e307f908ea9b06ad7adbbd9df0b8954ab51374faa8a98",
  warning: license_warning,
  unsafe: true
end

version "11.0.13+8" do
  source url: "https://github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.13%2B8/OpenJDK11U-jre_x64_linux_hotspot_11.0.13_8.tar.gz",
  sha256: "fb0a27e6e1f26a1ee79daa92e4cfe3ec0d676acfe114d99dd84b3414f056e8a0",
  warning: license_warning,
  unsafe: true
end

version "11.0.12+7" do
  source url: "https://github.com/adoptium/temurin11-binaries/releases/download/jdk-11.0.12%2B7/OpenJDK11U-jre_x64_linux_hotspot_11.0.12_7.tar.gz",
  sha256: "e813e270b7ea0a13f9c400ce5abd4cb811aacbd536b8909e6c7f0e346f78348c",
  warning: license_warning,
  unsafe: true
end

version "11.0.11+9" do
  source url: "https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.11%2B9/OpenJDK11U-jre_x64_linux_hotspot_11.0.11_9.tar.gz",
  sha256: "144f2c6bcf64faa32016f2474b6c01031be75d25325e9c3097aed6589bc5d548",
  warning: license_warning,
  unsafe: true
end

version "11.0.10+9" do
  source url: "https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.10%2B9/OpenJDK11U-jre_x64_linux_hotspot_11.0.10_9.tar.gz",
  sha256: "25fdcf9427095ac27c8bdfc82096ad2e615693a3f6ea06c700fca7ffb271131a",
  warning: license_warning,
  unsafe: true
end

relative_path "jdk-#{version}-jre"

build do
  mkdir "#{install_dir}/embedded/open-jre"
  sync  "#{project_dir}/", "#{install_dir}/embedded/open-jre"

  # Since we are using a precompiled-jre, it will look for zlib in the following path:
  # vagrant@default-ubuntu-1604:~$ chrpath jdk-11.0.4+11-jre/bin/java
  # jdk-11.0.4+11-jre/bin/java: RPATH=$ORIGIN/../lib/jli:$ORIGIN/../lib
  # This errors since it cannot find the libz.so.1 file that is installed
  # as a part of the omnibus environment.
  # We need to change the RPATH of the binary to be able to find omnibus installed zlib.

  new_rpath = "#{install_dir}/embedded/open-jre/lib/jli:#{install_dir}/embedded/lib:$ORIGIN/../lib"
  command "#{install_dir}/embedded/bin/patchelf --set-rpath #{new_rpath} #{install_dir}/embedded/open-jre/bin/*"
end
