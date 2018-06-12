#
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

# BEGIN AUTHZ CLEANUP
#
# Remove all traces of the legacy opscode-authz service that oc_bifrost
# replaces.
#
execute "/opt/opscode/bin/private-chef-ctl stop opscode-authz" do
  retries 20
end

runit_service "opscode-authz" do
  action :disable
end

directory "/opt/opscode/sv/opscode-authz" do
  action :delete
  recursive true
end
# END AUTHZ CLEANUP

oc_bifrost_dir = node['private_chef']['oc_bifrost']['dir']
oc_bifrost_bin_dir = File.join(oc_bifrost_dir, "bin")
oc_bifrost_log_dir = node['private_chef']['oc_bifrost']['log_directory']
oc_bifrost_sasl_log_dir = File.join(oc_bifrost_log_dir, "sasl")
[
  oc_bifrost_dir,
  oc_bifrost_bin_dir,
  oc_bifrost_log_dir,
  oc_bifrost_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

link "/opt/opscode/embedded/service/oc_bifrost/log" do
  to oc_bifrost_log_dir
end

oc_bifrost_config = File.join(oc_bifrost_dir, "sys.config")

template oc_bifrost_config do
  source "oc_bifrost.config.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "644"
  variables(node['private_chef']['oc_bifrost'].to_hash)
  notifies :restart, 'runit_service[oc_bifrost]' unless backend_secondary?
end

link "/opt/opscode/embedded/service/oc_bifrost/sys.config" do
  to oc_bifrost_config
end

vmargs_config = File.join(oc_bifrost_dir, "vm.args")

template vmargs_config do
  source "oc_bifrost.vm.args.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "644"
  notifies :restart, 'runit_service[oc_bifrost]' unless backend_secondary?
end

link "/opt/opscode/embedded/service/oc_bifrost/vm.args" do
  to vmargs_config
end

component_runit_service "oc_bifrost"
