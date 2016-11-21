#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

expander_dir = node['private_chef']['opscode-expander']['dir']
expander_etc_dir = File.join(expander_dir, "etc")
expander_log_dir = node['private_chef']['opscode-expander']['log_directory']

[ expander_dir, expander_etc_dir, expander_log_dir ].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

expander_config = File.join(expander_etc_dir, "expander.rb")

template expander_config do
  source "expander.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  options = node['private_chef']['opscode-expander'].to_hash
  variables(options)
  notifies :restart, 'runit_service[opscode-expander]' if is_data_master?
end

link "/opt/opscode/embedded/service/opscode-expander/conf/opscode-expander.rb" do
  to expander_config
end

component_runit_service "opscode-expander" do
  svlogd_size node['private_chef']['opscode-expander']['log_rotation']['file_maxbytes']
  svlogd_num node['private_chef']['opscode-expander']['log_rotation']['num_to_keep']
  ha node['private_chef']['opscode-expander']['ha']
end
