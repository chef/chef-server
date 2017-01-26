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

name "erlang-crypto2"
default_version "master"

source git: "https://github.com/jaym/erlang-crypto2.git"

# TODO get Jay to convert this to Apache 2
license "Reserved"
license_file "LICENSE"

# DEPENDENCIES
dependency "erlang"
dependency "rebar"

build do
  env = with_standard_compiler_flags(with_embedded_path)
  # profile_name = fips_mode? ? "fips" : "default"
  #
  # env['USE_SYSTEM_GECODE'] = "1"
  # env['REL_VERSION'] = "#{project.build_version}"
  # env['REBAR_PROFILE'] = profile_name
  #
  # make "omnibus", env: env
  command "rebar compile", env: env
  #
  # sync "#{project_dir}/_build/#{profile_name}/rel/oc_erchef/", "#{install_dir}/embedded/service/opscode-erchef/", exclude: ['**/.git', '**/.gitignore']
  # delete "#{install_dir}/embedded/service/opscode-erchef/log"
end
