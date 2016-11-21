# Copyright:: Copyright (c) 2014 Chef, Inc.
# License:: Apache License, Version 2.0
# Author:: Douglas Triggs <doug@chef.io>
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

name "ctl-man"
default_version "7b25fa4de4d6663dafe2cbe853ada29eee67a6a6"

license :project_license
skip_transitive_dependency_licensing true

dependency "private-chef-ctl"

version "7b25fa4de4d6663dafe2cbe853ada29eee67a6a6" do
  source md5: "0c64fd470f8dc780858f9c526dc4ee95"
end

source url: "https://raw.githubusercontent.com/opscode/chef-docs/#{version}/misc/chef-server-ctl.8"

build do
  mkdir "#{install_dir}/embedded/man/man8"
  copy "#{project_dir}/chef-server-ctl.8", "#{install_dir}/embedded/man/man8/"
end
