#
# Copyright 2017-2021 Chef Software, Inc.
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

name "berkshelf"
default_version "master"

license "Apache"
license_file "https://github.com/berkshelf/berkshelf/blob/master/LICENSE"

source git: "https://github.com/berkshelf/berkshelf.git"

dependency "ruby"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  bundle "install --without development changelog build docs", env: env

  gem "build berkshelf.gemspec", env: env
  gem "install berkshelf-*.gem", env: env
end
