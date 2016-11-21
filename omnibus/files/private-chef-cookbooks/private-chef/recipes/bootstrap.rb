#
# Author:: Adam Jacob <adam@chef.io>
# Copyright:: Copyright (c) 2011-2015 Chef Software, Inc.
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

# When we're running a new standalone install and are configured
# to install addons, do so now from the package repositories.
if (!OmnibusHelper.has_been_bootstrapped? &&
    node['private_chef']['topology'] == "standalone" &&
    node['private_chef']['addons']['install'])
  include_recipe "private-chef::add_ons_wrapper"
end

# These should always be running by this point, but let's be certain.
%w{postgresql oc_bifrost}.each do |service|
  execute "/opt/opscode/bin/chef-server-ctl start #{service}" do
    not_if { OmnibusHelper.has_been_bootstrapped? }
  end
end

ruby_block "bootstrap-chef-server-data" do
  block do
    ChefServerDataBootstrap.new(node).bootstrap
  end
  not_if { OmnibusHelper.has_been_bootstrapped? }
  notifies :restart, 'service[opscode-erchef]'
end

file OmnibusHelper.bootstrap_sentinel_file do
  owner "root"
  group "root"
  mode "0600"
  content "bootstrapped on #{DateTime.now} (you punk)" unless File.exists?(OmnibusHelper.bootstrap_sentinel_file)
end
