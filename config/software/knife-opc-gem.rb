#
# Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
# License:: Apache License, Version 2.0
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

name "knife-opc"
default_version "tc/add-password-cmd"

dependency "ruby"
dependency "rubygems"
dependency "bundler"

source :git => "git://github.com/opscode/knife-opc.git"

relative_path "knife-opc"

build do
  # Remove existing built gems if they exist in the current dir.
  # Add -f in case the file doesn't exist yet.
  command "rm -f knife-opc-*.gem"
  gem "build knife-opc.gemspec"

  gem "install knife-opc-*.gem"
end
