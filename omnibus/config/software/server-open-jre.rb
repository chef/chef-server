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

name "server-open-jre"
default_version "11"

unless _64_bit?
  raise "Server-open-jre can only be installed on x86_64 systems."
end

license "GPL-2.0 (with the Classpath Exception)"
license_file "LICENSE"

license_file "http://openjdk.java.net/legal/gplv2+ce.html"
skip_transitive_dependency_licensing true

whitelist_file "jre/bin/javaws"
whitelist_file "jre/bin/policytool"
whitelist_file "jre/lib"
whitelist_file "jre/plugin"
whitelist_file "jre/bin/appletviewer"

license_warning = "By including the JRE, you accept the terms of the OpenJDK Web Site Terms of Use Oracle Binary Code License Agreement for the Java SE Platform Products and JavaFX, which can be found at http://www.oracle.com/technetwork/java/javase/terms/license/index.html"
license_cookie = ""

version "11" do
  source url: "https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.4%2B11/OpenJDK11U-jre_x64_linux_hotspot_11.0.4_11.tar.gz",
         sha256: "70d2cc675155476f1d8516a7ae6729d44681e4fad5a6fc8dfa65cab36a67b7e0",
         cookie: license_cookie,
         warning: license_warning,
         unsafe:  true

  relative_path "jre11u11.0.4_11"
end

build do
  mkdir "#{install_dir}/embedded/open-jre"
  sync  "#{project_dir}/", "#{install_dir}/embedded/open-jre"
end
