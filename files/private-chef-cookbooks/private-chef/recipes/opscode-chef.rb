#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

private_chef_api_dir = node['private_chef']['opscode-chef']['dir']
private_chef_api_etc_dir = File.join(private_chef_api_dir, "etc")
private_chef_api_working_dir = File.join(private_chef_api_dir, "working")
private_chef_api_cache_dir = File.join(private_chef_api_dir, "cache")
private_chef_api_sandbox_dir = node['private_chef']['opscode-chef']['sandbox_path']
private_chef_api_checksum_dir = node['private_chef']['opscode-chef']['checksum_path']
private_chef_api_cookbook_cache_dir = File.join(private_chef_api_dir, "cookbooks_cache")
private_chef_api_log_dir = node['private_chef']['opscode-chef']['log_directory']

[
  private_chef_api_dir,
  private_chef_api_etc_dir,
  private_chef_api_working_dir,
  private_chef_api_cache_dir,
  private_chef_api_sandbox_dir,
  private_chef_api_checksum_dir,
  private_chef_api_cookbook_cache_dir,
  private_chef_api_log_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

chef_config = File.join(private_chef_api_etc_dir, "opscode-chef.conf")
env_config = File.join(private_chef_api_etc_dir, "#{node['private_chef']['opscode-chef']['environment']}.rb")
statsd_config = File.join(private_chef_api_etc_dir, "statsd_config.rb")

should_notify = OmnibusHelper.should_notify?("opscode-chef")

template chef_config do
  source "opscode-chef.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-chef'].to_hash)
  notifies :restart, 'service[opscode-chef]' if should_notify
end

link "/opt/opscode/embedded/service/opscode-chef/chef-server-api/config/opscode-chef.conf" do
  to chef_config
end

template env_config do
  source "opscode-chef-environment.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-chef'].to_hash)
  notifies :restart, 'service[opscode-chef]' if should_notify
end

link "/opt/opscode/embedded/service/opscode-chef/chef-server-api/config/environments/#{node['private_chef']['opscode-chef']['environment']}.rb" do
  to env_config
end

template statsd_config do
  source "statsd_config.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-chef'].to_hash)
  notifies :restart, 'service[opscode-chef]' if should_notify
end

link "/opt/opscode/embedded/service/opscode-chef/chef-server-api/statsd_config.rb" do
  to statsd_config
end

unicorn_config File.join(private_chef_api_etc_dir, "unicorn.rb") do
  listen node['private_chef']['opscode-chef']['listen'] => {
    :backlog => node['private_chef']['opscode-chef']['backlog'],
    :tcp_nodelay => node['private_chef']['opscode-chef']['tcp_nodelay']
  }
  worker_timeout node['private_chef']['opscode-chef']['worker_timeout']
  working_directory private_chef_api_working_dir
  worker_processes node['private_chef']['opscode-chef']['worker_processes']
  owner "root"
  group "root"
  mode "0644"
  log_listener true
  notifies :restart, 'service[opscode-chef]' if should_notify
end

runit_service "opscode-chef" do
  down node['private_chef']['opscode-chef']['ha']
  options({
    :log_directory => private_chef_api_log_dir,
    :svlogd_size => node['private_chef']['opscode-chef']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['opscode-chef']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl start opscode-chef" do
		retries 20
	end
end

add_nagios_hostgroup("opscode-chef")

