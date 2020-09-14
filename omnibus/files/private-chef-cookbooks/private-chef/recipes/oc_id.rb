#
# Author:: James Casey <james@chef.io>
# Copyright:: Copyright (c) 2014-2015 Chef Software, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# If no sign_up_url is defined, use the server URL.
#
# We don't have a clear way to detect whether Manage is installed or running or
# whether it's running on an alternate host/port, short of slurping the
# manage-running.json, so there's not an easy way to detect what the *actual*
# sign up URL is or whether we have one (which we won't if Manage is not
# installed), so we use the api_fqdn by default, which is the default location
# if Manage is installed with its default settings.
#
# In the long term, sign up is going to be moved into oc-id anyway, so this will
# not be an issue. In the short term, we will provide a way to disable sign up
# (see https://github.com/chef/oc-id/issues/41.)
#
# For now, if the sign up URL for Manage is anything different than what we
# default to here, you'll need to define it explicitly.
sign_up_url = node['private_chef']['oc_id']['sign_up_url']
unless sign_up_url
  sign_up_url = "https://#{node['private_chef']['api_fqdn']}/signup"
end

app_settings = {
  'chef' => {
    'endpoint' => "https://#{node['private_chef']['lb_internal']['vip']}:#{node['private_chef']['nginx']['ssl_port']}",
    'superuser' => 'pivotal',
    #
    # Why is this verify_none?
    #
    # Since we set the endpoint to 'localhost', even if we set the
    # trusted_cert_dir to include the user-provided or self-signed
    # cert in use by nginx, we will likely fail verification since
    # those are certs for the api_fqdn and not localhost.
    #
    'ssl_verify_mode' => 'verify_none',
  },
  'doorkeeper' => {
    'administrators' => node['private_chef']['oc_id']['administrators'] || [],
  },
  'sentry_dsn' => node['private_chef']['oc_id']['sentry_dsn'],
  'sign_up_url' => sign_up_url,
  'email_from_address' => node['private_chef']['oc_id']['email_from_address'],
  'origin' => node['private_chef']['oc_id']['origin'],
}

oc_id_dir = node['private_chef']['oc_id']['dir']
oc_id_config_dir = File.join(oc_id_dir, 'config')
oc_id_tmp_dir = File.join(oc_id_dir, 'tmp')
oc_id_log_dir = node['private_chef']['oc_id']['log_directory']
[
  oc_id_dir,
  oc_id_config_dir,
  oc_id_tmp_dir,
  oc_id_log_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

%w(log tmp).each do |dir|
  full_dir = "/opt/opscode/embedded/service/oc_id/#{dir}"
  directory full_dir do
    action :delete
    recursive true
    not_if { File.symlink?(full_dir) }
  end
end

link '/opt/opscode/embedded/service/oc_id/log' do
  to oc_id_log_dir
end

link '/opt/opscode/embedded/service/oc_id/tmp' do
  to oc_id_tmp_dir
end

# this is needed to allow for attributes which are lists to be marshalled
# properly
mutable_hash = JSON.parse(app_settings.dup.to_json)
file "#{oc_id_config_dir}/production.yml" do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '640'
  content mutable_hash.to_yaml
  notifies :restart, 'component_runit_service[oc_id]'
end

#
## Symlink settings file into the rails service directory
#
settings_file = '/opt/opscode/embedded/service/oc_id/config/settings/production.yml'
file settings_file do
  action :delete
  not_if { File.symlink?(settings_file) }
end

link settings_file do
  to "#{oc_id_config_dir}/production.yml"
end

template "#{oc_id_config_dir}/secret_token.rb" do
  source 'oc_id.secret_token.rb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '640'
  notifies :restart, 'component_runit_service[oc_id]'
end

secrets_file = '/opt/opscode/embedded/service/oc_id/config/initializers/secret_token.rb'
file secrets_file do
  action :delete
  not_if { File.symlink?(secrets_file) }
end

link secrets_file do
  to "#{oc_id_config_dir}/secret_token.rb"
end

template "#{oc_id_config_dir}/database.yml" do
  source 'oc_id.database.yml.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '640'
  notifies :restart, 'component_runit_service[oc_id]'
end

database_file = '/opt/opscode/embedded/service/oc_id/config/database.yml'
file database_file do
  action :delete
  not_if { File.symlink?(database_file) }
end

link database_file do
  to "#{node['private_chef']['oc_id']['dir']}/config/database.yml"
end

# Ensure log files are owned by opscode. In Chef 12.14 the svlogd
# service was changed to run as opscode rather than root. This is done
# as an execute to avoid issues with the `current` file not being
# there on the first run.
execute "chown -R #{OmnibusHelper.new(node).ownership['owner']}:#{OmnibusHelper.new(node).ownership['group']} #{oc_id_log_dir}" do
  only_if do
    begin
      ::File.stat(::File.join(oc_id_log_dir, 'current')).uid != Etc.getpwnam(OmnibusHelper.new(node).ownership['owner']).uid
    rescue Errno::ENOENT
      true
    end
  end
end

veil_helper_args = '--use-file -s chef-server.webui_key -s oc_id.sql_password -s oc_id.secret_key_base'
execute 'oc_id_schema' do
  command "veil-env-helper #{veil_helper_args} -- bundle exec --keep-file-descriptors rake db:migrate"
  cwd '/opt/opscode/embedded/service/oc_id'

  # There are other recipes that depend on having a VERSION environment
  # variable. If that environment variable is set when we run `rake db:migrate`,
  # and it is set to something the migrations do not expect, this will
  # break.
  #
  # We want to migrate to the latest version, which we can get by looking at the
  # date prefix of the latest file in the db/migrate directory.
  #
  # Also set the RAILS_ENV as is needed.
  environment('RAILS_ENV' => 'production',
              'VERSION' => `ls -1 /opt/opscode/embedded/service/oc_id/db/migrate | tail -n 1 | sed -e "s/_.*//g"`.chomp,
              'PATH' => '/opt/opscode/embedded/bin')
  sensitive false
  only_if { is_data_master? }
end

component_runit_service 'oc_id' do
  package 'private_chef'
end

if node['private_chef']['bootstrap']['enable']
  execute '/opt/opscode/bin/private-chef-ctl start oc_id' do
    retries 20
  end
end

# Take the existing oc_id.applications (with only a redirect_uri), ensure they
# exist in the database, and dump their data to /etc/opscode/oc-id-applications.
node['private_chef']['oc_id']['applications'].each do |name, app|
  oc_id_application name do
    write_to_disk node['private_chef']['insecure_addon_compat']
    redirect_uri app['redirect_uri']
    only_if { is_data_master? }
  end
end

nginx_dir = node['private_chef']['nginx']['dir']
nginx_etc_dir = File.join(nginx_dir, 'etc')
nginx_addon_dir = File.join(nginx_etc_dir, 'addon.d')

directory nginx_addon_dir do
  action :create
  recursive true
end

# LB configs
%w(upstreams external).each do |config|
  file = File.join(nginx_addon_dir, "40-oc_id_#{config}.conf")

  template file do
    source "oc_id.nginx-#{config}.conf.erb"
    owner 'root'
    group 'root'
    mode '0644'
    notifies :restart, 'component_runit_service[nginx]'
  end
end
