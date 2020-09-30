require 'chef/provider/lwrp_base'

class Chef
  class Provider
    class ElasticSearchIndex < Chef::Provider::LWRPBase
      provides :elasticsearch_index

      action :create do
        unless retry_index_exists?
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

      ## We check if the elasticsearch index exists before creating it.
      ## But Elasticsearch returns 404 for a short duration during startup before it stabilizes and can return a 200.
      ## As a result, we incorrectly try to recreate an index when it actually exists in the system.
      ## Re-creating an existing index is a 400 "Bad Request" error.
      def retry_index_exists?(count)
        solr_server.get("/#{new_resource.index_name}")
        true
      rescue Net::HTTPClientException => e
        raise unless (e.response && e.response.code) == '404'

        if count.positive?
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
  end
end
