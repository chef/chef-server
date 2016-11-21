#
# Author:: Marc Paradise <marc@chef.io>
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved

opscode_chef_mover_dir = node['private_chef']['opscode-chef-mover']['dir']
opscode_chef_mover_etc_dir = File.join(opscode_chef_mover_dir, "etc")
opscode_chef_mover_data_dir = node['private_chef']['opscode-chef-mover']['data_dir']
opscode_chef_mover_log_dir = node['private_chef']['opscode-chef-mover']['log_directory']
opscode_chef_mover_sasl_log_dir = File.join(opscode_chef_mover_log_dir, "sasl")
[
  opscode_chef_mover_dir,
  opscode_chef_mover_etc_dir,
  opscode_chef_mover_data_dir,
  opscode_chef_mover_log_dir,
  opscode_chef_mover_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-chef-mover/log" do
  to opscode_chef_mover_log_dir
end

mover_config = File.join(opscode_chef_mover_dir, "sys.config")

template mover_config do
  source "opscode-chef-mover.config.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "644"
  variables(node['private_chef']['opscode-chef-mover'].to_hash.merge({:helper => OmnibusHelper.new(node)}))
end

link "/opt/opscode/embedded/service/opscode-chef-mover/sys.config" do
  to mover_config
end

# We want the service defined, but dead
component_runit_service "opscode-chef-mover" do
  action :down
end
