# Copyright:: Copyright (c) 2014 Chef, Inc.
# License:: Apache License, Version 2.0
# Author:: Tyler Cloke <tyler@getchef.com>
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

# Make sure the man directories exist (they should but best not to assume).
%w[ /man /man/man8 ].each do |path|
  directory "#{node['private_chef']['user']['home']}/#{path}" do
    owner "root"
    group "root"
    action :create
  end
end

remote_file "#{node['private_chef']['user']['home']}/man/man8/chef-server-ctl.8" do
  source "https://github.com/opscode/chef-docs/raw/master/misc/chef-server-ctl.8"
  group "root"
  owner "root"
end
