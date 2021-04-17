#
# Copyright 2019 Chef Software, Inc.
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

name "knife-tidy-gem"
default_version "2.0.1"

license "Apache-2.0"
license_file "https://github.com/chef/knife-tidy/blob/master/LICENSE"
# knife-tidy gem does not have any dependencies. We only install it from
# rubygems here.
skip_transitive_dependency_licensing true

dependency "ruby"

build do
  env = with_standard_compiler_flags(with_embedded_path)

  gem "install knife-tidy" \
      " --version '#{version}'" \
      " --bindir '#{install_dir}/embedded/bin'" \
      "  --no-document", env: env
end
