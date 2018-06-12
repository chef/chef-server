#
# Author:: Stephen Delano (<stephen@chef.io>)
# Copyright:: 2012-2018 Chef Software, Inc.
#
# All Rights Reserved

upgrades_dir = node['private_chef']['upgrades']['dir']
upgrades_etc_dir = File.join(upgrades_dir, "etc")
upgrades_service_dir = "/opt/opscode/embedded/service/partybus"
[
  upgrades_dir,
  upgrades_etc_dir,
  upgrades_service_dir
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

partybus_config = File.join(upgrades_etc_dir, "config.rb")

db_service_name = "postgres"

# set the node role
node_role = node['private_chef']['role']

template partybus_config do
  source "partybus_config.rb.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode   "0644"
  variables(:connection_string => "postgres:/opscode_chef",
            :as_user => node['private_chef']['postgresql']['username'],
            :node_role => node_role,
            :db_service_name => db_service_name,
            :is_data_master => is_data_master?,
            :bootstrap_server => is_bootstrap_server?)
end

link "/opt/opscode/embedded/service/partybus/config.rb" do
  to partybus_config
end

execute "set initial migration level" do
  action :nothing
  command "cd /opt/opscode/embedded/service/partybus && ./bin/partybus init"
  subscribes :run, "file[#{OmnibusHelper.bootstrap_sentinel_file}]", :delayed
end

ruby_block 'migration-level file sanity check' do
  block do
    begin
      ::JSON.parse(::File.read('/var/opt/opscode/upgrades/migration-level'))
    rescue Exception => e
      message = <<-EOF
ERROR:
The /var/opt/opscode/upgrades/migration-level file is missing or corrupt!  Please read http://docs.chef.io/install_server_pre.html and correct this file before proceeding

* If this is a new installation:
  run: "cd /opt/opscode/embedded/service/partybus ; /opt/opscode/embedded/bin/bundle exec bin/partybus init"
* If you have upgraded a previous installation:
  copy the /var/opt/opscode/upgrades/migration-level file from a not-yet-upgraded FrontEnd node

Error message #{e}
EOF

      raise message
    end
  end
  not_if 'test -f /var/opt/opscode/upgrades/migration-level'
  action :nothing
  if OmnibusHelper.has_been_bootstrapped?
    subscribes :run, 'private-chef_pg_upgrade[upgrade_if_necessary]', :delayed
  else
    subscribes :run, 'execute[set initial migration level]', :immediately
  end
end
