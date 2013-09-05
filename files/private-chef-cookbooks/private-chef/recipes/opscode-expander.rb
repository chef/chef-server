#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

expander_dir = node['private_chef']['opscode-expander']['dir']
expander_etc_dir = File.join(expander_dir, "etc")
expander_log_dir = node['private_chef']['opscode-expander']['log_directory']
expander_reindexer_log_dir = node['private_chef']['opscode-expander']['reindexer_log_directory']

[ expander_dir, expander_etc_dir, expander_log_dir, expander_reindexer_log_dir ].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

expander_config = File.join(expander_etc_dir, "expander.rb")
reindexer_config = File.join(expander_etc_dir, "reindexer.rb")

template expander_config do
  source "expander.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  options = node['private_chef']['opscode-expander'].to_hash
  options['reindexer'] = false
  variables(options)
  notifies :restart, 'runit_service[opscode-expander]' if OmnibusHelper.should_notify?("opscode-expander")
end

link "/opt/opscode/embedded/service/opscode-expander/conf/opscode-expander.rb" do
  to expander_config
end

template reindexer_config do
  source "expander.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  options = node['private_chef']['opscode-expander'].to_hash
  options['reindexer'] = true
  variables(options)
  notifies :restart, 'runit_service[opscode-expander-reindexer]' if OmnibusHelper.should_notify?("opscode-expander-reindexer")
end

component_runit_service "opscode-expander"
component_runit_service "opscode-expander-reindexer" do
  log_directory expander_reindexer_log_dir
  enable node['private_chef']['opscode-expander']['enable']
  svlogd_size node['private_chef']['opscode-expander']['log_rotation']['file_maxbytes']
  svlogd_num node['private_chef']['opscode-expander']['log_rotation']['num_to_keep']
  ha node['private_chef']['opscode-expander']['ha']
end
