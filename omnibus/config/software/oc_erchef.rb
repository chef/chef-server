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

name "oc_erchef"

default_version "1.8.3"

source git: "https://github.com/opscode/oc_erchef"

dependency "erlang"
dependency "rebar"
dependency "gecode"
dependency "sqitch"
dependency "perl_pg_driver"

# RUBY DEPSOLVER - REMOVE FOR ROLLBACK #
dependency "ruby"
dependency "bundler"
# END RUBY DEPSOLVER #

build do
  env = with_standard_compiler_flags(with_embedded_path)
  env['USE_SYSTEM_GECODE'] = "1"

  make "distclean", env: env
  make "compile", env: env
  make "bundle", env: env
  make "rel", env: env

  sync "#{project_dir}/_rel/oc_erchef/", "#{install_dir}/embedded/service/opscode-erchef/", exclude: ['**/.git', '**/.gitignore']
  delete "#{install_dir}/embedded/service/opscode-erchef/log"
end
