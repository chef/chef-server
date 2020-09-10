#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: 2011-2018 Chef Software, Inc.
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

helper = OmnibusHelper.new(node)
opscode_erchef_dir = node['private_chef']['opscode-erchef']['dir']
opscode_erchef_log_dir = node['private_chef']['opscode-erchef']['log_directory']
opscode_erchef_sasl_log_dir = File.join(opscode_erchef_log_dir, 'sasl')
[
  opscode_erchef_dir,
  opscode_erchef_log_dir,
  opscode_erchef_sasl_log_dir,
].each do |dir_name|
  directory dir_name do
    owner helper.ownership['owner']
    group helper.ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

link '/opt/opscode/embedded/service/opscode-erchef/log' do
  to opscode_erchef_log_dir
end

ldap_authentication_enabled = helper.ldap_authentication_enabled?
# These values are validated and managed in libraries/private_chef.rb#gen_ldap
enable_ssl = ldap_authentication_enabled ? node['private_chef']['ldap']['enable_ssl'] : nil
ldap_encryption_type = ldap_authentication_enabled ? node['private_chef']['ldap']['encryption_type'] : nil

erchef_config = File.join(opscode_erchef_dir, 'sys.config')

template erchef_config do
  source 'oc_erchef.config.erb'
  owner helper.ownership['owner']
  group helper.ownership['group']
  mode '644'
  variables lazy {
    node['private_chef']['opscode-erchef'].to_hash.merge(ldap_enabled: ldap_authentication_enabled,
                                                         enable_ssl: enable_ssl,
                                                         ldap_encryption_type: ldap_encryption_type,
                                                         solr_elasticsearch_major_version: helper.elastic_search_major_version,
                                                         helper: helper)
  }
  notifies :run, 'execute[remove_erchef_siz_files]', :immediately
  notifies :restart, 'component_runit_service[opscode-erchef]'
end

# Erchef still ultimately uses disk_log [1] for request logging, and if
# you change the log file sizing in the configuration **without also
# issuing a call to disk_log:change_size/2, Erchef won't start.
#
# Since we currently don't perform live upgrades, we can fake this by
# removing the *.siz files, which is where disk_log looks to determine
# what size the log files should be in the first place.  If they're
# not there, then we just use whatever size is listed in the
# configuration.
#
# [1]: http://erlang.org/doc/man/disk_log.html
execute 'remove_erchef_siz_files' do
  command 'rm -f *.siz'
  cwd node['private_chef']['opscode-erchef']['log_directory']
  action :nothing
end

link '/opt/opscode/embedded/service/opscode-erchef/sys.config' do
  to erchef_config
end

vmargs_config = File.join(opscode_erchef_dir, 'vm.args')

template vmargs_config do
  source 'oc_erchef.vm.args.erb'
  owner helper.ownership['owner']
  group helper.ownership['group']
  mode '644'
  notifies :restart, 'component_runit_service[opscode-erchef]'
end

link '/opt/opscode/embedded/service/opscode-erchef/vm.args' do
  to vmargs_config
end

component_runit_service 'opscode-erchef' do
  control ['t']
end
