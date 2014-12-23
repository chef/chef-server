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

name "enterprise-chef-server-schema"
default_version "2.4.1"

source git: "git@github.com:opscode/enterprise-chef-server-schema.git"

dependency "sqitch"
dependency "perl_pg_driver"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  make "install", env: env
  sync  "#{project_dir}", "#{install_dir}/embedded/service/enterprise-chef-server-schema/", exclude: ['**/.git', '**/.gitignore']
end
