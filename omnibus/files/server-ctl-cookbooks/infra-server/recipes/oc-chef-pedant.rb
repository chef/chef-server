#
# Author:: Seth Chisamore (<schisamo@chef.io>)
# Copyright:: Chef Software, Inc.
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

pedant_dir = node['private_chef']['oc-chef-pedant']['dir']
pedant_etc_dir = File.join(pedant_dir, 'etc')
pedant_log_dir = node['private_chef']['oc-chef-pedant']['log_directory']
[
  pedant_dir,
  pedant_etc_dir,
  pedant_log_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

pedant_config = File.join(pedant_etc_dir, 'pedant_config.rb')

# Snag the first supported protocol version by our ruby installation
ssl_protocols = node['private_chef']['nginx']['ssl_protocols']
supported_versions = OpenSSL::SSL::SSLContext::METHODS
allowed_versions = ssl_protocols.split(/ /).select do |proto|
  supported_versions.include? proto.gsub('.', '_').to_sym
end

# In a healthy installation, we should be able to count on
# at least one shared protocol version. Leaving failure unhandled here,
# since it means that a pedant run is not possible.
ssl_version = allowed_versions.first.gsub('.', '_').to_sym

# In the older version of FIPS, TLS directives were not supported and hence it had to be connected to http
# currently FIPS supports https.
# If enable_non_ssl is set to false in nginx, the reindexing fails when it tries to make a http connection
# https://github.com/chef/chef-server/blob/fd06de39ffcc65e06f03c99a2301f295ad43e526/src/chef-server-ctl/lib/chef_server_ctl/config.rb#L23
reindex_endpoint = node['private_chef']['nginx']['enable_non_ssl'] ? 'http://127.0.0.1' : 'https://127.0.0.1'

template pedant_config do
  owner 'root'
  group 'root'
  mode  '0755'
  variables({
    api_url: node['private_chef']['oc-chef-pedant']['chef_server'] || OmnibusHelper.new(node).nginx_ssl_url,
    base_resource_url: node['private_chef']['opscode-erchef']['base_resource_url'],
    search_engine_url: OmnibusHelper.new(node).search_engine_url,
    search_provider: node['private_chef']['opscode-erchef']['search_provider'],
    search_auth_username: node['private_chef']['opscode-erchef']['search_auth_username'],
    search_auth_password: node['private_chef']['opscode-erchef']['search_auth_password'],
    opscode_account_internal_url: node['private_chef']['lb_internal']['vip'],
    opscode_account_internal_port: node['private_chef']['lb_internal']['account_port'],
    erchef_internal_vip: node['private_chef']['opscode-erchef']['vip'],
    erchef_internal_port: node['private_chef']['opscode-erchef']['port'],
    default_orgname: node['private_chef']['default_orgname'],
    hostname: node['hostname'],
    ssl_version: ssl_version,
    reindex_endpoint: reindex_endpoint,
    required_recipe_enabled: node['private_chef']['required_recipe']['enable'],
    chef_pgsql_collector: (node['private_chef']['postgresql']['enable'] &&
                               !node['private_chef']['postgresql']['external']),
    topology: node['private_chef']['topology'],
    role: node['private_chef']['role'],
  }.merge(node['private_chef']['oc-chef-pedant'].to_hash))
end
