#
# Cookbook Name:: oc-id
# Recipe:: default::_install
# Author:: Chris Nunciato <cnunciato@getchef.com>
# All rights reserved - Do Not Redistribute
#

include_recipe 'apt'
include_recipe 'build-essential'
include_recipe 'nodejs::install_from_binary'
include_recipe 'opscode-base'
include_recipe 'opscode-ruby'
include_recipe 'users::opscode'
include_recipe 'opscode-github'
include_recipe 'runit'
include_recipe 'unicorn'

env_attrs = data_bag_item('environments', node.chef_environment)['oc-id'] || {}
node_attrs = node['oc-id']
install_dir = node_attrs['install_dir']
group_name = node_attrs['group']
user_name = node_attrs['user']
release_dir = "#{install_dir}/current"

if node.chef_environment == 'dev'

  template "#{release_dir}/config/settings/#{env_attrs[:app_env]}.yml" do
    user user_name
    group group_name
    source 'settings.yml.erb'
    mode 0644
    variables env_attrs.to_hash.merge({
      :chef_environment => node.chef_environment
    })
  end

  file '/etc/opscode/webui_priv.pem' do
    content IO.read("#{release_dir}/config/webui_priv.pem")
  end

  execute 'install' do
    user user_name
    group group_name
    cwd release_dir
    command "bundle install --path vendor"
    notifies :run, 'execute[migrate]'
  end

  # execute 'migrate' do
  #   user user_name
  #   group group_name
  #   cwd release_dir
  #   command 'bundle exec rake db:migrate'
  #   notifies :restart, 'service[oc-id]'
  #   action :nothing
  # end

else

  # deploy_revision...

end

# unicorn_config '/etc/unicorn/oc-id.rb' do
#   listen node['oc-id']['port'] => { :backlog => 1024, :tcp_nodelay => true }
#   worker_timeout 120
#   preload_app true
#   worker_processes 6
#   owner user_name
#   group group_name
#   mode 0644
#   notifies :restart, 'service[oc-id]'
# end
#
# runit_service 'oc-id' do
#   options node_attrs.merge({'chef_environment' => node.chef_environment}).to_hash
#   owner user_name
#   group group_name
#   control(['t'])
# end
#
# template '/etc/rsyslog.d/30-oc-id.conf' do
#   source 'rsyslog-oc-id.conf.erb'
#   owner 'root'
#   group 'root'
#   mode 0644
#   notifies :restart, "service[rsyslog]"
# end
#
# template '/etc/logrotate.d/oc-id' do
#   source 'logrotate-oc-id.erb'
#   owner 'root'
#   group 'root'
#   mode 0644
# end
