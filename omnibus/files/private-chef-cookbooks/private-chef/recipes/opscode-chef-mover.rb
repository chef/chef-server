#
# Author:: Marc Paradise <marc@chef.io>
# Copyright:: 2013-2018 Chef Software, Inc.
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

opscode_chef_mover_dir = node['private_chef']['opscode-chef-mover']['dir']
opscode_chef_mover_etc_dir = File.join(opscode_chef_mover_dir, 'etc')
opscode_chef_mover_data_dir = node['private_chef']['opscode-chef-mover']['data_dir']
opscode_chef_mover_log_dir = node['private_chef']['opscode-chef-mover']['log_directory']
opscode_chef_mover_sasl_log_dir = File.join(opscode_chef_mover_log_dir, 'sasl')
[
  opscode_chef_mover_dir,
  opscode_chef_mover_etc_dir,
  opscode_chef_mover_data_dir,
  opscode_chef_mover_log_dir,
  opscode_chef_mover_sasl_log_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

link '/opt/opscode/embedded/service/opscode-chef-mover/log' do
  to opscode_chef_mover_log_dir
end

mover_config = File.join(opscode_chef_mover_dir, 'sys.config')

template mover_config do
  source 'opscode-chef-mover.config.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '644'
  variables(node['private_chef']['opscode-chef-mover'].to_hash.merge(helper: OmnibusHelper.new(node),
                                                                     redis_password: PrivateChef.credentials.get('redis_lb', 'password')))
end

link '/opt/opscode/embedded/service/opscode-chef-mover/sys.config' do
  to mover_config
end

vmargs_config = File.join(opscode_chef_mover_dir, 'vm.args')

template vmargs_config do
  source 'opscode-chef-mover.vm.args.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '644'
end

link '/opt/opscode/embedded/service/opscode-chef-mover/vm.args' do
  to vmargs_config
end

# We want the service defined, but dead
component_runit_service 'opscode-chef-mover' do
  action :create
end
