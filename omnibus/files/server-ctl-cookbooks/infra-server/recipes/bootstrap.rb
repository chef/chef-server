#
# Author:: Adam Jacob <adam@chef.io>
# Copyright:: Chef Software, Inc.
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

# When we're running a new standalone install and are configured
# to install addons, do so now from the package repositories.
if !OmnibusHelper.has_been_bootstrapped? &&
   node['private_chef']['topology'] == 'standalone' &&
   node['private_chef']['addons']['install']
  include_recipe 'infra-server::add_ons_wrapper'
end

# These should always be running by this point, but let's be certain.
%w(postgresql oc_bifrost).each do |service|
  # external postgres cannot be managed through chef-server-ctl.
  # But when we migrate the server manually (manual bootstrap), we touch the file `/var/opt/opscode/bootstrapped`
  # and create the file `/var/opt/opscode/upgrades/migration-level`
  # with the major and minor version of the migration file to be run.
  # ex : {"major":1,"minor":34}
  # After we do the needed migration, `OmnibusHelper.has_been_bootstrapped` will return `true`.
  execute "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/bin/chef-server-ctl start #{service}" do
    not_if { OmnibusHelper.has_been_bootstrapped? }
  end
end

ruby_block 'bootstrap-chef-server-data' do
  block do
    ChefServerDataBootstrap.new(node).bootstrap
  end
  not_if { OmnibusHelper.has_been_bootstrapped? }
  notifies :restart, 'component_runit_service[opscode-erchef]'
end

file OmnibusHelper.bootstrap_sentinel_file do
  owner 'root'
  group 'root'
  mode '0600'
  content "bootstrapped on #{DateTime.now} (you punk)" unless File.exist?(OmnibusHelper.bootstrap_sentinel_file)
end
