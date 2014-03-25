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

hostnames = data_bag_item('hostnames', node.chef_environment)

node_attrs = node['oc-id']
install_dir = node_attrs['install_dir']
group_name = node_attrs['group']
user_name = node_attrs['user']
rails_env = node_attrs['rails_env'] || 'production'
release_dir = "#{install_dir}/current"

directory install_dir do
  owner user_name
  group group_name
  mode 0755
  recursive true
end

if node.chef_environment == 'dev'

  package 'sqlite3'
  package 'sqlite3-doc'
  package 'libsqlite3-dev'

  execute 'replace key' do
    user 'root'
    group 'root'
    command 'chmod 644 /tmp/oc-id/config/webui_priv.pem && rm /etc/opscode/webui_priv.pem && cp /tmp/oc-id/config/webui_priv.pem /etc/opscode/webui_priv.pem'
    notifies :run, 'execute[deploy local]', :immediately
  end

  execute 'deploy local' do
    user user_name
    group group_name
    command "rm -rf #{release_dir} && mkdir -p #{release_dir} && cp -R /tmp/oc-id/* #{release_dir}"
    notifies :run, 'execute[install]', :immediately
    action :nothing
  end

  execute 'install' do
    user user_name
    group group_name
    cwd release_dir
    environment({ 'RAILS_ENV' => rails_env })
    command "bundle install --path ../bundle #{'--without test development' if rails_env == 'production'}"
    notifies :run, 'execute[precompile]', :immediately
    action :nothing
  end

  execute 'precompile' do
    user user_name
    group group_name
    cwd release_dir
    environment({ 'RAILS_ENV' => rails_env })
    command 'bundle exec rake assets:precompile'
    notifies :run, 'execute[migrate]', :immediately
    not_if
    action :nothing
  end

  execute 'migrate' do
    user user_name
    group group_name
    cwd release_dir
    environment({ 'RAILS_ENV' => rails_env })
    command 'bundle exec rake db:migrate'
    notifies :restart, 'service[oc-id]'
    action :nothing
  end

else

  # wip

  deploy_revision('oc-id') do
    repo 'git@github.com:opscode/oc-id.git'
    remote 'opscode'
    revision 'master'
    environment({ 'RAILS_ENV' => rails_env })
    user user_name
    group group_name
    deploy_to install_dir
    migration_command 'rake db:migrate --trace'
    migrate true

    symlink_before_migrate({
      'config/database.yml' => 'config/database.yml',
      "config/settings/#{rails_env}.yml" => "config/settings/#{rails_env}.yml"
    })

    symlinks({
      'assets' => 'public/assets'
    })

    before_symlink do

      template "#{release_dir}/config/settings/#{rails_env}.yml" do
        user user_name
        group group_name
        source 'settings.yml.erb'
        mode 0644
        variables node_attrs.merge({

        }).to_hash
      end

    end

    before_migrate do

      execute 'bundle install' do
        user user_name
        group group_name
        cwd install_dir
        command 'bundle install --path ../bundle --without test development'
      end

      execute 'bundle install' do
        user user_name
        group group_name
        cwd install_dir
        command 'bundle exec rake assets:precompile'
      end

    end

    action :deploy
    notifies :restart, 'service[oc-id]'
  end

end

template "#{release_dir}/config/settings/#{rails_env}.yml" do
  user user_name
  group group_name
  source 'settings.yml.erb'
  mode 0644
  variables node_attrs.merge({
    :hostnames => hostnames
  }).to_hash
end

unicorn_config '/etc/unicorn/oc-id.rb' do
  listen node['oc-id']['port'] => { :backlog => 1024, :tcp_nodelay => true }
  worker_timeout 120
  preload_app true
  worker_processes 6
  owner user_name
  group group_name
  mode 0644
  notifies :restart, 'service[oc-id]'
end

runit_service 'oc-id' do
  action :enable
  control(['t'])
  options node_attrs.merge({
    
  })
end

service('oc-id') do
  restart_command "#{node[:runit][:sv_bin]} -w 180 restart oc-id"
end

template '/etc/rsyslog.d/30-oc-id.conf' do
  source 'rsyslog-oc-id.conf.erb'
  owner 'root'
  group 'root'
  mode 0644
  notifies :restart, 'service[rsyslog]'
end

template '/etc/logrotate.d/oc-id' do
  source 'logrotate-oc-id.erb'
  owner 'root'
  group 'root'
  mode 0644
end

# deployment_notification 'deploy_revision[oc-id]' do
#   app_environment node.chef_environment
#   service_name 'oc-id'
#   estatsd_host vips['estatsd_host']
#   hipchat_key env['hipchat_key']
# end
