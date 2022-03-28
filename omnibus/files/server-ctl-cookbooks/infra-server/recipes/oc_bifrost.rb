#
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

oc_bifrost_dir = node['private_chef']['oc_bifrost']['dir']
oc_bifrost_bin_dir = File.join(oc_bifrost_dir, 'bin')
oc_bifrost_log_dir = node['private_chef']['oc_bifrost']['log_directory']
oc_bifrost_sasl_log_dir = File.join(oc_bifrost_log_dir, 'sasl')
[
  oc_bifrost_dir,
  oc_bifrost_bin_dir,
  oc_bifrost_log_dir,
  oc_bifrost_sasl_log_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

link "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/oc_bifrost/log" do
  to oc_bifrost_log_dir
end

oc_bifrost_config = File.join(oc_bifrost_dir, 'sys.config')

template oc_bifrost_config do
  source 'oc_bifrost.config.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '644'
  variables(node['private_chef']['oc_bifrost'].to_hash.merge(helper: OmnibusHelper.new(node)))
  notifies :restart, 'component_runit_service[oc_bifrost]'
end

link "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/oc_bifrost/sys.config" do
  to oc_bifrost_config
end

vmargs_config = File.join(oc_bifrost_dir, 'vm.args')

template vmargs_config do
  source 'oc_bifrost.vm.args.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '644'
  notifies :restart, 'component_runit_service[oc_bifrost]'
end

link "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/service/oc_bifrost/vm.args" do
  to vmargs_config
end

component_runit_service 'oc_bifrost' do
  control ['t']
end
