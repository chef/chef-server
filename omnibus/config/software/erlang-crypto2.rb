#
# Copyright 2012-2017 Chef Software, Inc.
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
default_version "er-459/update-crypto"

source git: "https://github.com/chef/erlang-crypto2.git"

license "BSD-3-Clause"
license_file "LICENSE"
# https://github.com/chef/license_scout/issues/61
skip_transitive_dependency_licensing true

dependency "erlang"
dependency "rebar"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  command "rebar compile", env: env

  crypto2_dir = "#{install_dir}/embedded/lib/erlang-crypto2"
  mkdir crypto2_dir
  copy "#{project_dir}/ebin", "#{crypto2_dir}/ebin"
  copy "#{project_dir}/priv", "#{crypto2_dir}/priv"
end
