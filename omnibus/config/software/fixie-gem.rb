#
# Copyright 2017-2018 Chef Software, Inc.
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

name "chef_fixie"
default_version "praj/update_chef_and_bundler"

license "Apache"
license_file "https://github.com/chef/fixie/blob/master/LICENSE"

source git: "https://github.com/chef/fixie.git"

dependency "ruby"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  bundle "install --without development test", env: env

  gem "build chef_fixie.gemspec", env: env
  gem "install chef_fixie-*.gem", env: env
end
