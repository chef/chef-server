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

name "bookshelf"
source path: "#{project.files_path}/../../src/bookshelf", options: {:exclude => ["_build"]}

license "Apache-2.0"
license_file "LICENSE.md"

dependency "erlang"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  env['REL_VERSION'] = "#{project.build_version}"

  make "omnibus", env: env

  sync "#{project_dir}/_build/default/rel/bookshelf/", "#{install_dir}/embedded/service/bookshelf/"
  delete "#{install_dir}/embedded/service/bookshelf/log"
end
