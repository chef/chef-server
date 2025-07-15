#
# Copyright:: Chef Software, Inc.
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

name "knife"
default_version "18.7.10"

license "Apache-2.0"
license_file "https://raw.githubusercontent.com/chef/chef/main/knife/LICENSE"

dependency "ruby"

skip_transitive_dependency_licensing "true"

# TODO: Switch back to gem installation when knife 18.7.10+ is published to RubyGems
# Original gem installation method (commented out):
# relative_path "knife-#{version}"
# build do
#     env = with_standard_compiler_flags(with_embedded_path)
#     v_opts = "--version '#{version}'" unless version.nil?
#     gem [
#       "install knife",
#       v_opts,
#       "--no-document",
#     ].compact.join(" "), env: env
#     command "knife --version", env: env
#     copy "#{install_dir}/embedded/bin/knife", "#{install_dir}/bin"
# end

# Temporary git source build until knife 18.7.10+ gem is published
source git: "https://github.com/chef/chef.git"

relative_path "chef"

build do
    env = with_standard_compiler_flags(with_embedded_path)

    # Build knife from the knife subdirectory
    command "cd knife && gem build knife.gemspec", env: env
    command "cd knife && gem install knife-#{version}.gem --no-document", env: env
  
    # confirm the install was successful
    command "knife --version", env: env
    copy "#{install_dir}/embedded/bin/knife", "#{install_dir}/bin"
end
