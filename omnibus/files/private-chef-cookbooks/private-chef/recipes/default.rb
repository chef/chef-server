#
# Copyright:: 2012-2018 Chef Software, Inc.
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

# Because these symlinks get removed during the postrm
# of the chef-server and private-chef packages, we should
# ensure that they're always here.
%w{private-chef-ctl chef-server-ctl}.each do |bin|
  link "/usr/bin/#{bin}" do
    to "/opt/opscode/bin/#{bin}"
  end
end

# Ensure that all our Omnibus-ed binaries are the ones that get used;
# much better than having to specify this on each resource!
ENV['PATH'] = "/opt/opscode/bin:/opt/opscode/embedded/bin:#{ENV['PATH']}"

directory "/etc/opscode" do
  owner "root"
  group "root"
  mode "0755"
  action :nothing
end.run_action(:create)

directory "/etc/opscode/logrotate.d" do
  owner "root"
  group "root"
  mode "0755"
  action :nothing
end.run_action(:create)

include_recipe "private-chef::plugin_discovery"
include_recipe "private-chef::plugin_config_extensions"
include_recipe "private-chef::config"

if node['private_chef']['fips_enabled']
  include_recipe "private-chef::fips"
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

if OmnibusHelper.has_been_bootstrapped? or
    BootstrapPreflightValidator.new(node).bypass_bootstrap?
  node.override['private_chef']['bootstrap']['enable'] = false
end

# Create the Chef User and private keys (pivotal/webui)
include_recipe "private-chef::users"
include_recipe "private-chef::private_keys"

# merge xdarklaunch values into the disk-based darklaunch
# so that we have a single source of truth for xdl-related
# values
darklaunch_values = node['private_chef']['dark_launch']
  .merge(node['private_chef']['lb']['xdl_defaults'])
  .to_hash

file "/etc/opscode/dark_launch_features.json" do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0644"
  content Chef::JSONCompat.to_json_pretty(darklaunch_values)
end

directory "/etc/chef" do
  owner "root"
  group OmnibusHelper.new(node).ownership['group']
  mode "0775"
  action :create
end

directory "/var/opt/opscode" do
  owner "root"
  group "root"
  mode "0755"
  recursive true
  action :create
end

directory "/var/log/opscode" do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0755"
  action :create
end

include_recipe "enterprise::runit"
include_recipe "private-chef::sysctl-updates"
# Run plugins first, mostly for ha
include_recipe "private-chef::plugin_chef_run"

if node['private_chef']['use_chef_backend']
  include_recipe "private-chef::haproxy"
end

include_recipe "private-chef::fix_permissions"

# Configure Services
[
  "postgresql",
  "oc_bifrost",
  "oc_id",
  "opscode-solr4",
  "opscode-expander",
  "bookshelf",
  "opscode-erchef",
  "bootstrap",
  "opscode-chef-mover",
  "redis_lb",
  "nginx",
  "rabbitmq"
].each do |service|
  if node["private_chef"][service]["external"]
    begin
      # Perform any necessary configuration of the external service:
      include_recipe "private-chef::#{service}-external"
    rescue Chef::Exceptions::RecipeNotFound
      raise "#{service} has the 'external' attribute set true, but does not currently support being run externally."
    end
    # Disable the actual local service since what is enabled
    # is an externally managed version. Given that bootstrap and
    # opscode-expander are not externalizable, don't need special
    # handling for them as we do in the normal disable case below.
    runit_service service do
      action :disable
    end
  else
    if node["private_chef"][service]["enable"]
      include_recipe "private-chef::#{service}"
    else
      # bootstrap isn't a service, nothing to disable.
      next if service == 'bootstrap'
      # All non-enabled services get disabled;
      runit_service service do
        action :disable
      end
    end
  end
end

include_recipe "private-chef::cleanup"

if darklaunch_values["actions"] && node['private_chef']['insecure_addon_compat']
  include_recipe "private-chef::actions"
else
  include_recipe "private-chef::remove_actions"
end

include_recipe "private-chef::private-chef-sh"
include_recipe "private-chef::oc-chef-pedant"
include_recipe "private-chef::log_cleanup"
include_recipe "private-chef::partybus"
include_recipe "private-chef::ctl_config"
include_recipe "private-chef::disable_chef_server_11"

file "/etc/opscode/chef-server-running.json" do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"
  content lazy { OmnibusHelper.chef_server_running_content(node) }
end

ruby_block "print reconfigure warnings" do
  block do
    ChefServer::Warnings.print_warnings
  end
end
