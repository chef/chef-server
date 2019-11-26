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

expander_dir = node['private_chef']['opscode-expander']['dir']
expander_etc_dir = File.join(expander_dir, 'etc')
expander_log_dir = node['private_chef']['opscode-expander']['log_directory']

[ expander_dir, expander_etc_dir, expander_log_dir ].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

expander_config = File.join(expander_etc_dir, 'expander.rb')

template expander_config do
  source 'expander.rb.erb'
  owner 'root'
  group 'root'
  mode '0644'
  options = node['private_chef']['opscode-expander'].to_hash
  variables(options)
  notifies :restart, 'component_runit_service[opscode-expander]' if is_data_master?
end

link '/opt/opscode/embedded/service/opscode-expander/conf/opscode-expander.rb' do
  to expander_config
end

component_runit_service 'opscode-expander' do
  svlogd_size node['private_chef']['opscode-expander']['log_rotation']['file_maxbytes']
  svlogd_num node['private_chef']['opscode-expander']['log_rotation']['num_to_keep']
  ha node['private_chef']['opscode-expander']['ha']
end
