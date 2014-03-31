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

package 'sqlite3'
package 'sqlite3-doc'
package 'libsqlite3-dev'

hostnames = data_bag_item('hostnames', node.chef_environment)
vips = data_bag_item('vips', node.chef_environment)
env = data_bag_item('environments', node.chef_environment)

node_attrs = node['oc-id']
install_dir = node_attrs['install_dir']
group_name = node_attrs['group']
user_name = node_attrs['user']
rails_env = node_attrs['rails_env'] || 'production'
revision = env['oc-id']['revision'] || 'master'
remote = env['default-remote'] || 'opscode'

dev = node.chef_environment == 'dev'
local = node['oc-id']['source'] == 'local'

if dev
  
  execute 'replace key' do
    user 'root'
    group 'root'
    command 'chmod 644 /tmp/oc-id/config/webui_priv.pem && rm /etc/opscode/webui_priv.pem && cp /tmp/oc-id/config/webui_priv.pem /etc/opscode/webui_priv.pem'
    notifies :run, 'execute[deploy]', :immediately if local
  end

  if local

    release_dir = "#{install_dir}/current"

    directory release_dir do
      owner user_name
      group group_name
      mode 0755
      recursive true
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

    execute 'deploy' do
      user user_name
      group group_name
      command "rm -rf #{release_dir} && mkdir -p #{release_dir} && cp -R /tmp/oc-id/* #{release_dir}"
      notifies :run, 'execute[install]', :immediately
      action :nothing
    end
  end
end

if !local

  %w{shared shared/system shared/config shared/pids shared/log shared/vendor}.each do |dir|
    directory "#{install_dir}/#{dir}" do
      owner user_name
      group group_name
      mode 02775
    end
  end

  deploy_revision('oc-id') do
    user user_name
    group group_name
    repo "git@github.com:#{remote}/oc-id.git"
    revision revision
    remote remote
    deploy_to install_dir
    migration_command 'bundle exec rake db:migrate'
    migrate true

    environment({
      'LC_CTYPE' => 'en_US.UTF-8',
      'LANG' => 'en_US.UTF-8',
      'PATH' => "/usr/local/bin:#{ENV['PATH']}",
      'RAILS_ENV' => rails_env
    })

    symlink_before_migrate({})
    symlinks('system' => 'public/system', 'pids' => 'tmp/pids', 'log' => 'log', 'bundle' => 'vendor/bundle')

    before_migrate do

      template "#{release_path}/config/settings/#{rails_env}.yml" do
        user user_name
        group group_name
        source 'settings.yml.erb'
        mode 0644
        variables node_attrs.merge({
          :hostnames => hostnames
        }).to_hash
      end

      run 'bundle install --deployment --without test development'
    end

    before_restart do
      run 'bundle exec rake assets:precompile'
    end

    action dev ? :force_deploy : :deploy
    notifies :restart, 'service[oc-id]'
  end

  deployment_notification 'deploy_revision[oc-id]' do
    app_environment node.chef_environment
    service_name 'oc-id'
    estatsd_host vips['estatsd_host']
    hipchat_key env['hipchat_key']
  end
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
  options node_attrs.merge({})
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
