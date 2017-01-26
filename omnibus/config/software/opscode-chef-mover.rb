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

name "opscode-chef-mover"
source path: "#{project.files_path}/../../src/chef-mover", options: {:exclude => ["_build"]}
license :project_license
dependency "erlang"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  profile_name = "default"

  env['USE_SYSTEM_GECODE'] = "1"
  env['REL_VERSION'] = "#{project.build_version}"
  env['REBAR_PROFILE'] = profile_name

  make "omnibus", env: env

  sync "#{project_dir}/_build/#{profile_name}/rel/mover/", "#{install_dir}/embedded/service/opscode-chef-mover/"
  delete "#{install_dir}/embedded/service/opscode-chef-mover/log"

  mkdir "#{install_dir}/embedded/service/opscode-chef-mover/scripts"
  copy "scripts/migrate", "#{install_dir}/embedded/service/opscode-chef-mover/scripts/"
  copy "scripts/check_logs.rb", "#{install_dir}/embedded/service/opscode-chef-mover/scripts/"
  command "chmod ugo+x #{install_dir}/embedded/service/opscode-chef-mover/scripts/*"
end
