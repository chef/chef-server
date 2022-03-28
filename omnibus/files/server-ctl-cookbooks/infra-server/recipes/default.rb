#
# Copyright:: Chef Software, Inc.
# Author:: Adam Jacob (<adam@chef.io>)
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

require 'uuidtools'
require 'openssl'
require 'English' # for $CHILD_STATUS, etc variables 

# Because these symlinks get removed during the postrm
# of the chef-server and private-chef packages, we should
# ensure that they're always here.
%w(private-chef-ctl chef-server-ctl).each do |bin|
  link "/usr/bin/#{bin}" do
    to "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/bin/#{bin}"
  end
end

# Ensure that all our Omnibus-ed binaries are the ones that get used;
# much better than having to specify this on each resource!
ENV['PATH'] = "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/bin:/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/bin:#{ENV['PATH']}"

directory "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}" do
  owner 'root'
  group 'root'
  mode '0755'
  action :nothing
end.run_action(:create)

directory "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/logrotate.d" do
  owner 'root'
  group 'root'
  mode '0755'
  action :nothing
end.run_action(:create)

include_recipe 'infra-server::config'

if node['private_chef']['fips_enabled']
  include_recipe 'infra-server::fips'
end

# Warn about deprecated opscode_webui settings
opscode_webui_deprecation_notice = OpscodeWebuiDeprecationNotice.new(
  PrivateChef['opscode_webui']
)

log 'opscode_webui deprecation notice' do
  message opscode_webui_deprecation_notice.message
  level :warn
  only_if { opscode_webui_deprecation_notice.applicable? }
end

if OmnibusHelper.has_been_bootstrapped? ||
   BootstrapPreflightValidator.new(node).bypass_bootstrap?
  node.override['private_chef']['bootstrap']['enable'] = false
end

# Create the Chef User and private keys (pivotal/webui)
include_recipe 'infra-server::users'
include_recipe 'infra-server::private_keys'

# merge xdarklaunch values into the disk-based darklaunch
# so that we have a single source of truth for xdl-related
# values
darklaunch_values = node['private_chef']['dark_launch']
                    .merge(node['private_chef']['lb']['xdl_defaults'])
                    .to_hash

file "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/dark_launch_features.json" do
  owner OmnibusHelper.new(node).ownership['owner']
  group 'root'
  mode '0644'
  content Chef::JSONCompat.to_json_pretty(darklaunch_values)
end

directory '/etc/chef' do
  owner 'root'
  group OmnibusHelper.new(node).ownership['group']
  mode '0775'
  action :create
end

directory "/var/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}" do
  owner 'root'
  group 'root'
  mode '0755'
  recursive true
  action :create
end

directory "/var/log/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}" do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0755'
  action :create
end

include_recipe 'enterprise::runit'
include_recipe 'infra-server::sysctl-updates'

if node['private_chef']['use_chef_backend']
  # Ensure internal elasticsearch is not enabled
  # if we are in the chef_backend configuration
  node.override['private-chef']['elasticsearch']['enable'] = false
  include_recipe 'infra-server::haproxy'
end

include_recipe 'infra-server::fix_permissions'

# Configure Services
%w(
  postgresql
  oc_bifrost
  oc_id
  elasticsearch
  bookshelf
  opscode-erchef
  nginx
  bootstrap
  opscode-chef-mover
  redis_lb
).each do |service|
  if node['private_chef'][service]['external']
    begin
      # Perform any necessary configuration of the external service:
      include_recipe "infra-server::#{service}-external"
    rescue Chef::Exceptions::RecipeNotFound
      raise "#{service} has the 'external' attribute set true, but does not currently support being run externally."
    end
    # Disable the actual local service since what is enabled is an
    # externally managed version. Given that bootstrap isn't
    # externalizable, we don't need special handling for it as we do
    # in the normal disable case below.
    component_runit_service service do
      action :disable
    end
  elsif node['private_chef'][service]['enable']
    if service == 'elasticsearch' && node['private_chef']['opscode-erchef']['search_provider'] == 'opensearch' && node['private_chef']['opensearch']['external']
      include_recipe 'infra-server::opensearch-external'
    else
      include_recipe "infra-server::#{service}"
    end
  else
    # bootstrap isn't a service, nothing to disable.
    next if service == 'bootstrap'

    # All non-enabled services get disabled;
    component_runit_service service do
      action :disable
    end
  end
end

include_recipe 'infra-server::cleanup'

include_recipe 'infra-server::private-chef-sh'
include_recipe 'infra-server::oc-chef-pedant'
include_recipe 'infra-server::log_cleanup'
include_recipe 'infra-server::partybus'
include_recipe 'infra-server::ctl_config'

file "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/chef-server-running.json" do
  owner OmnibusHelper.new(node).ownership['owner']
  group 'root'
  mode '0600'
  content lazy { OmnibusHelper.chef_server_running_content(node) }
end

ruby_block 'print reconfigure warnings' do
  block do
    ChefServer::Warnings.print_warnings
  end
end
