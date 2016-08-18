#
# Copyright 2012-2014 Chef Software, Inc.
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

name "openresty-lpeg"
default_version "0.12"

license "MIT"
license_file "lpeg.html"
skip_transitive_dependency_licensing true

source url: "http://www.inf.puc-rio.br/~roberto/lpeg/lpeg-#{version}.tar.gz",
       md5: "4abb3c28cd8b6565c6a65e88f06c9162"

dependency "openresty"

relative_path "lpeg-#{version}"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  if ppc64? || ppc64le? || ohai['kernel']['machine'] == "s390x"
    make "LUADIR=#{install_dir}/embedded/include", env: env
  else
    make "LUADIR=#{install_dir}/embedded/luajit/include/luajit-2.1", env: env
  end
  command "install -p -m 0755 lpeg.so #{install_dir}/embedded/lualib", env: env
end
