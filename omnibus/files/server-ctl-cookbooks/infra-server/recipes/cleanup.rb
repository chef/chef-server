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

# pre Chef Infra Server 14.0 services

component_runit_service 'opscode-solr4' do
  action :disable
end

directory "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/sv/opscode-solr4" do
  recursive true
  action :delete
end

component_runit_service 'rabbitmq' do
  action :disable
end

directory "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/sv/rabbitmq" do
  recursive true
  action :delete
end

::Dir.glob(::File.join(node['private_chef']['user']['home'], '/elasticsearch/lib/log4j*2.11.1*')).each do |filename|
  file filename do
    action :delete
  end
end

# This component had been removed - it is no longer used
# in any migrations in after 12.17.15
component_runit_service 'opscode-chef-mover' do
  action :disable
end

directory "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/sv/opscode-chef-mover" do
  recursive true
  action :delete
end

[
  "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/sv/elasticsearch",
  "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/elasticsearch",
  "/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/service/elasticsearch",
  "/var/log/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/elasticsearch/",
].each do |dir_name|
  directory dir_name do
    recursive true
    action :delete
  end
end
