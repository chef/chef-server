#
# Cookbook:: chef-server-deploy
# Recipe:: omnibus_standalone
#
# Copyright:: Copyright 2017 Chef Software, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

node.default["#{Chef::Dist::Server::SHORT}-deploy"]['automate_server_fqdn'] = server_fqdn_for('automate')
node.default["#{Chef::Dist::Server::SHORT}-deploy"]['chef_server_fqdn'] = server_fqdn_for('#{Chef::Dist::Server::SHORT}')
node.default["#{Chef::Dist::Server::SHORT}-deploy"]['supermarket_fqdn'] = server_fqdn_for('supermarket')
node.default["#{Chef::Dist::Server::SHORT}-deploy"]['enable_liveness_agent'] = environment == 'delivered'

# By default we daemonize chef-client across all of our infrastructure nodes. We
# do not want this behavior on the Chef Server instances as we want the pipeline
# to control the roll out of changes.
edit_resource(:service, 'chef-client') do
  action [:disable, :stop]
end

################################################################################
# Chef Server
################################################################################

cert_filename = "/etc/opscode/#{node["#{Chef::Dist::Server::SHORT}-deploy"]['chef_cert_filename']}"
key_filename  = "/etc/opscode/#{node["#{Chef::Dist::Server::SHORT}-deploy"]['chef_key_filename']}"
automate_liveness_recipe_path = '/etc/opscode/automate-liveness-recipe.rb'

directory '/etc/opscode' do
  mode '0755'
end

file cert_filename do
  mode '0600'
  content citadel[node["#{Chef::Dist::Server::SHORT}-deploy"]['chef_cert_filename']]
end

file key_filename do
  mode '0600'
  content citadel[node["#{Chef::Dist::Server::SHORT}-deploy"]['chef_key_filename']]
end

if node["#{Chef::Dist::Server::SHORT}-deploy"]['enable_liveness_agent']
  remote_file automate_liveness_recipe_path do
    source liveness_agent_recipe_url
    mode '644'
    notifies :reconfigure, "chef_ingredient[#{Chef::Dist::Server::SHORT}]"
  end
end

chef_ingredient "#{Chef::Dist::Server::SHORT}" do
  channel omnibus_channel_for_environment("#{Chef::Dist::Server::SHORT}")
  version version_for_environment("#{Chef::Dist::Server::SHORT}")
  # We need to reconfigure after an install/upgrade
  notifies :reconfigure, "chef_ingredient[#{Chef::Dist::Server::SHORT}]"

  action :upgrade
end

template "/etc/opscode/#{Chef::Dist::Server::SHORT}.rb" do
  source "#{Chef::Dist::Server::SHORT}.rb.erb"
  variables(
    chef_server_deploy: node["#{Chef::Dist::Server::SHORT}-deploy"],
    chef_cert_filename: cert_filename,
    chef_key_filename: key_filename,
    required_recipe_path: automate_liveness_recipe_path,
    enable_data_collector: environment == 'delivered'
  )
  notifies :reconfigure, "chef_ingredient[#{Chef::Dist::Server::SHORT}]", :immediately
end

################################################################################
# Push Jobs Server
################################################################################
chef_ingredient 'push-jobs-server' do
  channel :stable
  platform_version_compatibility_mode true
  action :upgrade
  # We need to reconfigure after an install/upgrade
  notifies :reconfigure, "chef_ingredient[push-jobs-server]"
  notifies :reconfigure, "chef_ingredient[#{Chef::Dist::Server::SHORT}]"
end

ingredient_config 'push-jobs-server' do
  notifies :reconfigure, 'chef_ingredient[push-jobs-server]'
end

################################################################################
# Chef Manage
################################################################################
node.default["#{Chef::Dist::Server::SHORT}-deploy"]['manage-config']['saml.enabled'] = node["#{Chef::Dist::Server::SHORT}-deploy"]['enable_saml']
node.default["#{Chef::Dist::Server::SHORT}-deploy"]['manage-config']['saml.issuer_url'] = "'https://#{node["#{Chef::Dist::Server::SHORT}-deploy"]['automate_server_fqdn']}/api/v0'"

chef_ingredient 'manage' do
  channel :stable
  accept_license true
  config node['#{Chef::Dist::Server::SHORT}-deploy']['manage-config'].map { |k, v| "#{k} #{v}" }.join("\n")
  action :upgrade
  # We need to reconfigure after an install/upgrade
  notifies :reconfigure, 'chef_ingredient[manage]'
end

if node['#{Chef::Dist::Server::SHORT}-deploy']['enable_saml']
  chef_server_secret 'saml.client_id' do
    value 'manage'
  end

  chef_server_secret 'saml.client_secret' do
    value citadel['manage_oidc_client_secret']
  end
end

ingredient_config 'manage' do
  notifies :reconfigure, 'chef_ingredient[manage]'
end

omnibus_service '#{Chef::Dist::Server::SHORT}' do
  action :nothing
end

#########################################################################
# Expose the Supermarket OC ID creds
#########################################################################
# This is a hack to allow the Supermarket instance to get the oc-id app details
# via a HTTP call.

omnibus_service '#{Chef::Dist::Server::SHORT}/nginx' do
  action :nothing
end

file '/var/opt/opscode/nginx/etc/addon.d/99-supermarket-credentials_external.conf' do
  content(lazy do
    <<-EOF
location /supermarket-credentials {
  types { }
  default_type application/json;
  return 200 '#{oc_id_applciation_config('supermarket')}';
}
EOF
  end)
  notifies :restart, 'omnibus_service[#{Chef::Dist::Server::SHORT}/nginx]'
end

#########################################################################
# Create default users and organizations
#########################################################################

# This ensures the admin user's password is set to a known value across all
# environments. The password can be found in the `chef-cd Chef Server Admin User`
# record in LastPass.
chef_user 'admin' do
  first_name 'Chef-CD'
  last_name 'Admin'
  email 'eng-services-ops#chef-cd@chef.io'
  password citadel['chef_server_admin_password'].chomp
  serveradmin true
end

# Users
{
  'delivery' => {
    password: citadel['automate_admin_password'].chomp,
    serveradmin: true,
  },
  'expeditor' => {
    password: citadel['expeditor_password'].chomp,
    serveradmin: false,
  },
}.each do |user_name, user_options|
  chef_user user_name do
    first_name 'Chef'
    last_name user_name.capitalize
    email "eng-services-ops##{user_name}@chef.io"
    password user_options[:password]
    serveradmin user_options[:serveradmin]
  end

  file "/etc/opscode/users/#{user_name}-2.pem.pub" do
    content OpenSSL::PKey::RSA.new(citadel["#{user_name}.pem"]).public_key.to_s
    user 'root'
    group 'root'
    mode '0644'
  end

  # TODO: Do this in resource
  execute "add-#{user_name}-user-key" do
    command lazy { "#{Chef::Dist::Server::SHORT}-ctl add-user-key #{user_name} --public-key-path /etc/opscode/users/#{user_name}-2.pem.pub --key-name #{user_name}-2" }
    not_if "#{Chef::Dist::Server::SHORT}-ctl list-user-keys #{user_name} | grep #{user_name}-2"
  end
end

# Orgs
%w(
  chef
  chef-cd
).each do |org_name|
  chef_org org_name do
    admins %w(
      admin
      delivery
    )
  end
end
