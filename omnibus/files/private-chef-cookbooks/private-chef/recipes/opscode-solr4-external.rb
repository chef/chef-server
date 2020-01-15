# Author:: Steven Danna
# Copyright:: 2015-2018 Chef Software, Inc.
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
case node['private_chef']['opscode-erchef']['search_provider']
when 'solr'
  Chef::Log.warn('External Solr Support does not include configuring the Solr schema.')
when 'elasticsearch'
  elasticsearch_index 'chef' do
    server_url node['private_chef']['opscode-solr4']['external_url']
    index_definition(helper.create_elasticsearch_index)
  end
end
