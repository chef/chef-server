#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: 2012-2018 Chef Software, Inc.
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

keepalived_dir = node['private_chef']['keepalived']['dir']
keepalived_etc_dir = File.join(keepalived_dir, "etc")
keepalived_bin_dir = File.join(keepalived_dir, "bin")
keepalived_log_dir = node['private_chef']['keepalived']['log_directory']

[ keepalived_dir, keepalived_etc_dir, keepalived_bin_dir ].each do |dir|
  directory dir do
    owner "root"
    recursive true
    mode "0755"
  end
end

directory keepalived_log_dir do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  recursive true
  mode node['private_chef']['service_dir_perms']
end

vrrp_instance_password = PrivateChef.credentials.get('keepalived', 'vrrp_instance_password')

template File.join(keepalived_etc_dir, "keepalived.conf") do
  source "keepalived.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['keepalived'].to_hash.merge('vrrp_instance_password' => vrrp_instance_password))
  notifies :restart, 'runit_service[keepalived]'
end

template File.join(keepalived_bin_dir, "cluster.sh") do
  source "cluster.sh.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['keepalived'].to_hash)
end

component_runit_service "keepalived"
