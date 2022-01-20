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
