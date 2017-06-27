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

name "knife-ec-backup"
default_version "master"
source git: "https://github.com/chef/knife-ec-backup.git"


license "Apache-2.0"
license_file "LICENSE"
# No need to collect license information for dependencies since
# --ignore-dependencies flag is passed in during gem install.
skip_transitive_dependency_licensing true

dependency "pg-gem"
dependency "sequel-gem"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  gem "build knife-ec-backup.gemspec", env: env
  gem "install knife-ec-backup*.gem --no-rdoc --no-ri --ignore-dependencies", env: env
end
