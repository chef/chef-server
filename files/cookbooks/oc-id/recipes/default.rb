#
# Cookbook Name:: oc-id
# Recipe:: default
# Author:: Chris Nunciato <cnunciato@getchef.com>
# All rights reserved - Do Not Redistribute
#

include_recipe 'apt'
include_recipe 'build-essential'
include_recipe 'opscode-base'
include_recipe 'opscode-ruby'
include_recipe 'users::opscode'
include_recipe 'opscode-github'
include_recipe 'runit'

node_attrs = node['oc-id']
install_dir = node_attrs['install_dir']
app_path = node_attrs['app_path']
release_dir = "#{install_dir}/current/#{app_path}"
user_name = node_attrs['user']
group_name = node_attrs['group']

directory install_dir do
  owner user_name
  group group_name
  mode 0755
  recursive true
end

runit_service 'oc-id' do
  options node_attrs.merge({'chef_environment' => node.chef_environment}).to_hash
  owner user_name
  group group_name
  control(['t'])
end

template '/etc/rsyslog.d/30-oc-id.conf' do
  source 'rsyslog-oc-id.conf.erb'
  owner 'root'
  group 'root'
  mode 0644
  notifies :restart, "service[rsyslog]"
end

template '/etc/logrotate.d/oc-id' do
  source 'logrotate-oc-id.erb'
  owner 'root'
  group 'root'
  mode 0644
end

# Then like start the app, and stuff.