require 'chef/provider/lwrp_base'

class Chef
  class Provider
    class ElasticSearchIndex < Chef::Provider::LWRPBase
      provides :elasticsearch_index

      action :create do
        unless index_exists?
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

      def index_exists?
        solr_server.get("/#{new_resource.index_name}")
        true
      rescue Net::HTTPClientException => e
        if e.response && e.response.code == '404'
          false
        else
          raise
        end
      end

      def solr_server
        @solr_server ||= Chef::HTTP::SimpleJSON.new(new_resource.server_url)
      end
    end
  end
end
