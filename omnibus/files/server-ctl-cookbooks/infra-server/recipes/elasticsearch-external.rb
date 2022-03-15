# Author:: Steven Danna
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

case node['private_chef']['opscode-erchef']['search_provider']
when 'solr'
  Chef::Log.warn('External Solr Support does not include configuring the Solr schema.')
when 'elasticsearch'
  include_recipe 'infra-server::elasticsearch_index'
when 'opensearch' # Recipe name should be changed ?
  include_recipe 'infra-server::opensearch_index'
end
