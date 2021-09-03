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

provides :elasticsearch_index

property :index_name, String, name_property: true

property :server_url, kind_of: String

property :index_definition, kind_of: Hash

action :create do
  unless retry_index_exists?(4)
    converge_by "Creating elasticsearch index #{new_resource.index_name}" do
      solr_server.put(new_resource.index_name, new_resource.index_definition)
    end
  end
end

action :destroy do
  if index_exists?
    converge_by "Deleting elasticsearch index #{new_resource.index_name}" do
      solr_server.delete(new_resource.index_name)
    end
  end
end

action_class do
  ## We check if the Elasticsearch index exists before creating it.
  ## But Elasticsearch returns 404 for a short duration during startup before it stabilizes and can return a 200.
  ## As a result, we incorrectly try to recreate an index when it actually exists in the system.
  ## Re-creating an existing index is a 400 "Bad Request" error.
  def retry_index_exists?(count)
    solr_server.get("/#{new_resource.index_name}")
    true
  rescue Errno::ECONNREFUSED => e
    if count.positive?
      Chef::Log.debug('Could not connect to Elasticsearch, retrying in 5 seconds')
      sleep 5
      retry_index_exists?(count - 1)
    else
      Chef::Log.fatal('Could not connect to Elasticsearch')
      false
    end
  rescue Net::HTTPClientException => e
    raise unless e.response && e.response.code == '404'

    if count.positive?
      Chef::Log.debug("Elasticsearch returned 404 for /#{new_resource.index_name}, retrying in 5 second")
      sleep 5
      retry_index_exists?(count - 1)
    else
      false
    end
  end

  def solr_server
    @solr_server ||= Chef::HTTP::SimpleJSON.new(new_resource.server_url)
  end
end
