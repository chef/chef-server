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

helper = OmnibusHelper.new(node)
opensearch_index 'chef' do
  server_url lazy { helper.search_engine_url }
  index_definition lazy { helper.search_engine_index_definition }
  # opensearch_user lazy { node['private_chef']['opscode-erchef']['opensearch_user'] }
  # opensearch_password lazy { helper.opensearch_password}
  opensearch_auth lazy { helper.search_engine_auth_header }
  search_engine_url lazy { helper.search_engine_url }
end
