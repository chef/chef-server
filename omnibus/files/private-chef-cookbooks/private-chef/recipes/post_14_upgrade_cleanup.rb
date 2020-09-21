#
# Copyright:: 2020 Chef Software, Inc.
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

private_chef_package_cleaner 'opscode-solr4' do
  directories [
    node['private_chef']['opscode-solr4']['dir'],
    node['private_chef']['opscode-solr4']['data_dir'],
    node['private_chef']['opscode-solr4']['log_directory'],
  ]
end

private_chef_package_cleaner 'rabbitmq' do
  directories [
    node['private_chef']['rabbitmq']['dir'],
    node['private_chef']['rabbitmq']['data_dir'],
    node['private_chef']['rabbitmq']['log_directory']
  ]
end

private_chef_package_cleaner 'opscode-expander' do
  directories [
    node['private_chef']['opscode-expander']['dir'],
    node['private_chef']['opscode-expander']['log_directory']
  ]
end
