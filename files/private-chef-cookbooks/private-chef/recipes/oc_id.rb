#
# Author:: James Casey <james@getchef.com>
# Copyright:: Copyright (c) 2014 Chef, Inc.
#
# All Rights Reserved

app_settings = {
  'chef' => {
    'endpoint' => "https://#{node['private_chef']['lb_internal']['vip']}",
    'superuser' => 'pivotal',
    'key_path' => '/etc/opscode/webui_priv.pem'
  },
  'doorkeeper' => {
    'administrators' => node['private_chef']['oc_id']['administrators'] || []
  }
}

oc_id_dir = node['private_chef']['oc_id']['dir']
oc_id_config_dir = File.join(oc_id_dir, "config")
oc_id_tmp_dir = File.join(oc_id_dir, "tmp")
oc_id_log_dir = node['private_chef']['oc_id']['log_directory']
[
  oc_id_dir,
  oc_id_config_dir,
  oc_id_tmp_dir,
  oc_id_log_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    group node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

%w{log tmp}.each do |dir|
  full_dir = "/opt/opscode/embedded/service/oc_id/#{dir}"
  directory full_dir do
    action :delete
    recursive true
    not_if  { File.symlink?(full_dir) }
  end
end

link "/opt/opscode/embedded/service/oc_id/log" do
  to oc_id_log_dir
end

link "/opt/opscode/embedded/service/oc_id/tmp" do
  to oc_id_tmp_dir
end

# this is needed to allow for attributes which are lists to be marshalled
# properly
mutable_hash = JSON.parse(app_settings.dup.to_json)
file "#{oc_id_config_dir}/production.yml" do
  user node['private_chef']['user']['username']
  group node['private_chef']['user']['username']
  mode '640'
  content mutable_hash.to_yaml
  notifies :restart, 'runit_service[oc_id]' unless backend_secondary?
end

#
## Symlink settings file into the rails service directory
#
settings_file = "/opt/opscode/embedded/service/oc_id/config/settings/production.yml"
file settings_file do
  action :delete
  not_if  { File.symlink?(settings_file) }
end
link settings_file do
  to "#{oc_id_config_dir}/production.yml"
end

template "#{oc_id_config_dir}/secret_token.rb" do
  source "oc_id.secret_token.rb"
  user node['private_chef']['user']['username']
  group node['private_chef']['user']['username']
  mode '640'
  notifies :restart, 'runit_service[oc_id]' unless backend_secondary?
end
secrets_file = "/opt/opscode/embedded/service/oc_id/config/initializers/secret_token.rb"
file secrets_file do
  action :delete
  not_if  { File.symlink?(secrets_file) }
end
link secrets_file do
  to "#{oc_id_config_dir}/secret_token.rb"
end

template "#{oc_id_config_dir}/database.yml" do
  source "oc_id.database.yml.erb"
  user node['private_chef']['user']['username']
  group node['private_chef']['user']['username']
  mode '640'
  notifies :restart, 'runit_service[oc_id]' unless backend_secondary?
end
database_file = "/opt/opscode/embedded/service/oc_id/config/database.yml"
file database_file do
  action :delete
  not_if  { File.symlink?(database_file) }
end
link database_file do
  to "#{node['private_chef']['oc_id']['dir']}/config/database.yml"
end

execute "oc_id_schema" do
  command "bundle exec rake db:migrate"
  path ["/opt/opscode/embedded/bin"]
  cwd "/opt/opscode/embedded/service/oc_id"
  environment({"RAILS_ENV" => "production"})
  only_if { is_data_master? }
end

component_runit_service "oc_id" do
  package 'private_chef'
end

if node['private_chef']['bootstrap']['enable']
  execute "/opt/opscode/bin/private-chef-ctl start oc_id" do
    retries 20
  end
end

nginx_dir = node['private_chef']['nginx']['dir']
nginx_etc_dir = File.join(nginx_dir, "etc")
nginx_addon_dir = File.join(nginx_etc_dir, "addon.d")

directory nginx_addon_dir do
  action :create
  recursive true
end

# LB configs
["upstreams", "external"].each do |config|
  file = File.join(nginx_addon_dir, "40-oc_id_#{config}.conf")

  template file do
    source "oc_id.nginx-#{config}.conf.erb"
    owner "root"
    group "root"
    mode "0644"
    notifies :restart, 'runit_service[nginx]' unless backend_secondary?
  end
end
