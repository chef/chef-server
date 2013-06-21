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
  notifies :restart, 'service[opscode-expander]' if OmnibusHelper.should_notify?("opscode-expander")
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
  notifies :restart, 'service[opscode-expander-reindexer]' if OmnibusHelper.should_notify?("opscode-expander-reindexer")
end

runit_service "opscode-expander" do
  down node['private_chef']['opscode-expander']['ha']
  options({
    :log_directory => expander_log_dir,
    :svlogd_size => node['private_chef']['opscode-expander']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['opscode-expander']['log_rotation']['num_to_keep']
  }.merge(params))
end

runit_service "opscode-expander-reindexer" do
  down node['private_chef']['opscode-expander']['ha']
  options({
    :log_directory => expander_reindexer_log_dir,
    :svlogd_size => node['private_chef']['opscode-expander']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['opscode-expander']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl start opscode-expander" do
		retries 20
	end
	execute "/opt/opscode/bin/private-chef-ctl start opscode-expander-reindexer" do
		retries 20
	end
end
