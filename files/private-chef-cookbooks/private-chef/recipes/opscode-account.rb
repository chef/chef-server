#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

private_chef_account_dir = node['private_chef']['opscode-account']['dir']
private_chef_account_etc_dir = File.join(private_chef_account_dir, "etc")
private_chef_account_working_dir = File.join(private_chef_account_dir, "working")
private_chef_account_log_dir = node['private_chef']['opscode-account']['log_directory']

[
  private_chef_account_dir,
  private_chef_account_etc_dir,
  private_chef_account_working_dir,
  private_chef_account_log_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

rebuild_config = File.join(private_chef_account_etc_dir, "rebuild.conf.rb")
env_config = File.join(private_chef_account_etc_dir, "#{node['private_chef']['opscode-account']['environment']}.rb")
statsd_config = File.join(private_chef_account_etc_dir, "statsd_config.rb")

should_notify = OmnibusHelper.should_notify?("opscode-account")

template rebuild_config do
  source "rebuild.conf.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables node['private_chef']['opscode-account'].to_hash
end

template env_config do
  source "opscode-account-config.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-account'].to_hash.merge(:ldap_enabled => ldap_authentication_enabled?))
  notifies :restart, 'service[opscode-account]' if should_notify
end

link "/opt/opscode/embedded/service/opscode-account/config/environments/#{node['private_chef']['opscode-account']['environment']}.rb" do
  to env_config
end

template statsd_config do
  source "statsd_config.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-account'].to_hash)
  notifies :restart, 'service[opscode-account]' if should_notify
end

link "/opt/opscode/embedded/service/opscode-account/statsd_config.rb" do
  to statsd_config
end

unicorn_config File.join(private_chef_account_etc_dir, "unicorn.rb") do
  listen node['private_chef']['opscode-account']['listen'] => {
    :backlog => node['private_chef']['opscode-account']['backlog'],
    :tcp_nodelay => node['private_chef']['opscode-account']['tcp_nodelay']
  }
  worker_timeout node['private_chef']['opscode-account']['worker_timeout']
  working_directory private_chef_account_working_dir
  worker_processes node['private_chef']['opscode-account']['worker_processes']
  owner "root"
  group "root"
  mode "0644"
  log_listener true
  notifies :restart, 'service[opscode-account]' if should_notify
end

runit_service "opscode-account" do
  down node['private_chef']['opscode-account']['ha']
  options({
    :log_directory => private_chef_account_log_dir,
    :svlogd_size => node['private_chef']['opscode-account']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['opscode-account']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl start opscode-account" do
		retries 20
	end
end

add_nagios_hostgroup("opscode-account")

