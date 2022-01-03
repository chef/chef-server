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

provides :opensearch_index

property :index_name, String, name_property: true

property :server_url, String

property :index_definition, Hash

property :opensearch_user, String

property :search_engine_url, String

action :init do
  converge_by "Creating new user for erchef" do
    create_opensearch_user
  end
end

action :create do
  unless retry_index_exists?(4)
    converge_by "Creating opensearch index #{new_resource.index_name}" do
      search_engine_server.put(new_resource.index_name, new_resource.index_definition, search_engine_auth_header)
    end
  end
end

action :destroy do
  if index_exists?
    converge_by "Deleting elasticsearch index #{new_resource.index_name}" do
      search_engine_server.delete(new_resource.index_name, search_engine_auth_header)
    end
  end
end

action_class do
  def create_opensearch_user
    max_requests = 5
    current_request = 1
    begin
      body = {
        password => PrivateChef.credentials.get('opscode_erchef', 'opensearch_password'),
        backend_roles => ["admin"],
        description => "Chef Server erchef user"
      }
      search_engine_server.put("_plugins/_security/api/internalusers/#{new_resource.opensearch_user}", body, search_engine_auth_header)
    rescue => e
      # Perform a blind rescue because Net:HTTP throws a variety of exceptions - some of which are platform specific.
      if current_request == max_requests
        raise "Failed to connect to opensearch service at #{new_resource.search_engine_url}: #{e}"
      else
        # Chef HTTP logs the details in the debug log.
        Chef::Log.error "Failed to connect to opensearch service #{current_request}/#{max_requests}. Retrying."
        current_request += 1
        sleep(current_request * 2) # Exponential back-off.
        retry
      end
    end
  end

  ## We check if the Elasticsearch index exists before creating it.
  ## But Elasticsearch returns 404 for a short duration during startup before it stabilizes and can return a 200.
  ## As a result, we incorrectly try to recreate an index when it actually exists in the system.
  ## Re-creating an existing index is a 400 "Bad Request" error.
  def retry_index_exists?(count)
    search_engine_server.get("/#{new_resource.index_name}", search_engine_auth_header)
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

  def search_engine_server
    @search_engine_server ||= Chef::HTTP::SimpleJSON.new(new_resource.server_url)
  end

  def search_engine_auth_header
    password = PrivateChef.credentials.get('opscode_erchef', 'opensearch_password')
    auth = Base64.strict_encode64("#{new_resource.opensearch_user}:#{password}")
    {Authorization: "Basic #{auth}"}
  end
end
