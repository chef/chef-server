#
# Author:: Marc Paradise <marc@opscode.com>
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
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-chef-mover/log" do
  to opscode_chef_mover_log_dir
end

template "/opt/opscode/embedded/service/opscode-chef-mover/bin/mover" do
  source "opscode-chef-mover.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['opscode-chef-mover'].to_hash)
  notifies :restart, 'runit_service[opscode-chef-mover]' if OmnibusHelper.should_notify?("opscode-chef-mover")
end

mover_config = File.join(opscode_chef_mover_etc_dir, "sys.config")

template mover_config do
  source "opscode-chef-mover.config.erb"
  mode "644"
  variables(node['private_chef']['opscode-chef-mover'].to_hash)
  notifies :restart, 'service[opscode-chef-mover]' if OmnibusHelper.should_notify?("opscode-chef-mover")
end

link "/opt/opscode/embedded/service/opscode-chef-mover/etc/sys.config" do
  to mover_config
end

component_runit_service "opscode-chef-mover"

