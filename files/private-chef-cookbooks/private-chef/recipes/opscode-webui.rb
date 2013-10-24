#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

private_chef_webui_dir = node['private_chef']['opscode-webui']['dir']
private_chef_webui_etc_dir = File.join(private_chef_webui_dir, "etc")
private_chef_webui_working_dir = File.join(private_chef_webui_dir, "working")
private_chef_webui_tmp_dir = File.join(private_chef_webui_dir, "tmp")
private_chef_webui_log_dir = node['private_chef']['opscode-webui']['log_directory']

[
  private_chef_webui_dir,
  private_chef_webui_etc_dir,
  private_chef_webui_working_dir,
  private_chef_webui_tmp_dir,
  private_chef_webui_log_dir,

].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

env_config = File.join(private_chef_webui_etc_dir, "#{node['private_chef']['opscode-webui']['environment']}.rb")
session_store_config = File.join(private_chef_webui_etc_dir, "session_store.rb")
secret_token_config = File.join(private_chef_webui_etc_dir, "secret_token.rb")

template env_config do
  source "opscode-webui-config.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-webui'].to_hash.merge(:ldap_enabled => ldap_authentication_enabled?))
  notifies :restart, 'runit_service[opscode-webui]' unless backend_secondary?
end

link "/opt/opscode/embedded/service/opscode-webui/config/environments/#{node['private_chef']['opscode-webui']['environment']}.rb" do
  to env_config
end

template session_store_config do
  source "session_store.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-webui'].to_hash)
  notifies :restart, 'runit_service[opscode-webui]' unless backend_secondary?
end

link "/opt/opscode/embedded/service/opscode-webui/config/initializers/session_store.rb" do
  to session_store_config
end

template secret_token_config do
  source "secret_token.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-webui'].to_hash)
  notifies :restart, 'runit_service[opscode-webui]' unless backend_secondary?
end

link "/opt/opscode/embedded/service/opscode-webui/config/initializers/secret_token.rb" do
  to secret_token_config
end

unicorn_config File.join(private_chef_webui_etc_dir, "unicorn.rb") do
  listen node['private_chef']['opscode-webui']['listen'] => {
    :backlog => node['private_chef']['opscode-webui']['backlog'],
    :tcp_nodelay => node['private_chef']['opscode-webui']['tcp_nodelay']
  }
  worker_timeout node['private_chef']['opscode-webui']['worker_timeout']
  working_directory private_chef_webui_working_dir
  worker_processes node['private_chef']['opscode-webui']['worker_processes']
  owner "root"
  group "root"
  mode "0644"
  notifies :restart, 'runit_service[opscode-webui]' unless backend_secondary?
end

link "/opt/opscode/embedded/service/opscode-webui/tmp" do
  to private_chef_webui_tmp_dir
end

execute "chown -R #{node['private_chef']['user']['username']} /opt/opscode/embedded/service/opscode-webui/public"

component_runit_service "opscode-webui"
