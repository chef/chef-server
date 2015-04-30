#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

keepalived_dir = node['private_chef']['keepalived']['dir']
keepalived_etc_dir = File.join(keepalived_dir, "etc")
keepalived_bin_dir = File.join(keepalived_dir, "bin")
keepalived_log_dir = node['private_chef']['keepalived']['log_directory']

[ keepalived_dir, keepalived_etc_dir, keepalived_bin_dir ].each do |dir|
  directory dir do
    owner "root"
    recursive true
    mode "0755"
  end
end

directory keepalived_log_dir do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  recursive true
  mode node['private_chef']['service_dir_perms']
end

template File.join(keepalived_etc_dir, "keepalived.conf") do
  source "keepalived.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['keepalived'].to_hash)
  notifies :restart, 'runit_service[keepalived]'
end

template File.join(keepalived_bin_dir, "cluster.sh") do
  source "cluster.sh.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['keepalived'].to_hash)
end

component_runit_service "keepalived"
