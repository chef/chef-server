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

# Capture old node attribute values (if there are any, that is) in
# case we need them for comparison purposes for making changes or
# updates
#
# TODO: extract this into something that add-ons can use; no sense
# cargo-culting it around everywhere
server_running_json = "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/chef-server-running.json"
if File.exist?(server_running_json)
  old_config = JSON.parse(IO.read(server_running_json))

  # We're stashing these outside the "private_chef" attributes tree to
  # prevent us from carrying them along forever when we write out the
  # chef-server-running.json file at the end of the run.
  #
  # For example, to access the old version of the attribute
  # ['private_chef']['foo']['bar'], you'll look at
  # ['previous_run']['foo']['bar'].
  #
  # Take care to check that ['previous_run'] exists, though, otherwise
  # you'll run into trouble doing the initial Chef run.
  #
  # TODO: Provide an API for getting this information
  node.consume_attributes('previous_run' => old_config['private_chef'])
end

chef_server_path = "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/chef-server.rb"
chef_server_json_path = "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/chef-server.json"
private_chef_path = "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/private-chef.rb"

if File.exist?(chef_server_json_path) &&
   !(File.exist?(private_chef_path) || File.exist?())
  Chef::Log.fatal("Configuration via #{chef_server_json_path} is not supported. Please use #{chef_server_path}")
  exit!(1)
else
  PrivateChef[:node] = node
  private_chef_rb_exists = File.exist?(private_chef_path)
  private_chef_rb_not_symlink = !File.symlink?(private_chef_path)
  chef_server_rb_exists = File.exist?(chef_server_path)

  if chef_server_rb_exists
    chef_server_rb_not_empty = !File.zero?(chef_server_path)
  end

  # Things are a bit tricky here, because there are multiple scenarios.  But the
  # upshot is, if we have a private-chef.rb and don't have (or have an empty)
  # chef-server.rb, then copy it over and link back.  Otherwise warn.
  if private_chef_rb_exists && private_chef_rb_not_symlink && chef_server_rb_exists &&
     chef_server_rb_not_empty
    Chef::Log.warn("#{private_chef_path} is deprecated and should be removed. Using #{chef_server_path}")
  elsif private_chef_rb_exists && private_chef_rb_not_symlink
    Chef::Log.warn("Moving to #{chef_server_path} for configuration - #{private_chef_path} is deprecated.")
    FileUtils.mv(private_chef_path, chef_server_path)
    FileUtils.ln_s(chef_server_path, private_chef_path)
    chef_server_rb_exists = true
  end

  if File.exist?(chef_server_json_path)
    Chef::Log.warn("Ignoring unsupported configuration file #{chef_server_json_path}.")
    Chef::Log.warn("Using #{chef_server_path} instead.")
  end

  if chef_server_rb_exists
    # Restrict 'other' permissions on chef-server.rb as it may contain sensitive info,
    # such as the LDAP bind password. Using FileUtils to fix, as a file resource only
    # sets permissions in absolute mode. This addresses config files created before the
    # fix was implemented in the postinst script.
    FileUtils.chmod('o-rwx', chef_server_path)
    PrivateChef.from_file(chef_server_path)
  end

  if PrivateChef['opscode_solr4']['external']
    node.default['private_chef']['elasticsearch']['enable'] = false
  end

  # Bail out if something is wrong in our configuration.
  # NOTE: Over time, we can move the validation done in private_chef.rb
  #       here as well.
  #
  # Preflight checks are executed prior to merging chef-server.rb (here as PrivateChef)
  # into the node, so that we can validate the settings explicitly set by
  # the customer, and be aware of those that were not set.
  #
  # If preflight checks fail, they will abort immediately with a detailed error
  # message, and without a stacktrace to clutter the screen.
  PreflightChecks.new(node).run!

  node.consume_attributes(PrivateChef.generate_config(node['fqdn']))
end
