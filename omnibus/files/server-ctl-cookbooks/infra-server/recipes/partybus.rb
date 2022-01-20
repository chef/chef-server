#
# Author:: Stephen Delano (<stephen@chef.io>)
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

upgrades_dir = node['private_chef']['upgrades']['dir']
upgrades_etc_dir = File.join(upgrades_dir, 'etc')
upgrades_service_dir = "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/partybus"
[
  upgrades_dir,
  upgrades_etc_dir,
  upgrades_service_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

partybus_config = File.join(upgrades_etc_dir, 'config.rb')

db_service_name = 'postgres'

# set the node role
node_role = node['private_chef']['role']

template partybus_config do
  source 'partybus_config.rb.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0644'
  variables(node_role: node_role,
            db_service_name: db_service_name,
            is_data_master: is_data_master?,
            bootstrap_server: is_bootstrap_server?)
end

link "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/partybus/config.rb" do
  to partybus_config
end

execute 'set initial migration level' do
  action :nothing
  command "cd /opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/partybus && ./bin/partybus init"
  subscribes :run, "file[#{OmnibusHelper.bootstrap_sentinel_file}]", :delayed
end

ruby_block 'migration-level file check' do
  block do
    begin
      ::JSON.parse(::File.read("/var/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/upgrades/migration-level"))
    rescue Exception => e
      message = <<~EOF
        ERROR:
        The /var/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/upgrades/migration-level file is missing or corrupt! Please read https://docs.chef.io/server/install_server_pre/ and correct this file before proceeding

        * If this is a new installation:
          run: "cd /opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/partybus ; /opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/bin/bundle exec bin/partybus init"
        * If you have upgraded a previous installation:
          copy the /var/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/upgrades/migration-level file from a not-yet-upgraded FrontEnd node

        Error message #{e}
      EOF

      raise message
    end
  end
  not_if { ::File.exist?("/var/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/upgrades/migration-level") }
  action :nothing
  if OmnibusHelper.has_been_bootstrapped?
    subscribes :run, 'pg_upgrade[upgrade_if_necessary]', :delayed
  else
    subscribes :run, 'execute[set initial migration level]', :immediately
  end
end
