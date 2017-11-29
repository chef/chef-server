#
# Author:: Stephen Delano (<stephen@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
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
