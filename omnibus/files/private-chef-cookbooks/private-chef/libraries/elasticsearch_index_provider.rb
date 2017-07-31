require 'chef/provider/lwrp_base'

class Chef
  class Provider
    class ElasticSearchIndex < Chef::Provider::LWRPBase
      use_inline_resources if defined?(:use_inlined_resources)

      action :create_or_update do
        if index_exists?
          if index_changed?
            converge_block_for_update
          end
        else
          converge_block_for_create
        end
      end

      action :destroy do
        converge_block_for_destroy if index_exists?
      end

      def existing_index_mappings
        unless @orig_mappings
          index = JSON.parse(solr_server.get("#{new_resource.index_name}"))
          @orig_mappings = index[new_resource.index_name]['mappings']
            solr_server.put("#{url}?update_all_types",
                            Chef::JSONCompat.to_json(mapping))
          end
        end

      end

      def converge_block_for_create
        converge_by "Creating elasticsearch index #{new_resource.index_name}" do
          solr_server.put(new_resource.index_name, Chef::JSONCompat.to_json(new_resource.index_definition))
        end
      end

      def converge_block_for_destroy
        converge_by "Deleting elasticsearch index #{new_resource.index_name}" do
          solr_server.delete(new_resource.index_name)
        end
      end

      def index_changed?
        new_index_mappings = new_resource.index_definition['mappings']
        eq = field_maps_are_equal?(existing_index_mappings,
                                   new_index_mappings)
      end

      def index_exists?
        !existing_index_mappings.nil?
      end

      def field_maps_are_equal?(map1, map2)
        return false if map1.length != map2.length
        map1.each do |k, v|
          v2 = map2[k]
          # This handles the nil case too:
          return false unless v.class == v2.class
          if v.class == Hash
            return false unless field_maps_are_equal?(v, v2)
          else
            # Other values back are limited to directly comparaible key-values
            return false unless v == v2
          end
        end
        return true
      end


      def solr_server
        @solr_server ||= Chef::HTTP.new(new_resource.server_url)
      end
    end
  end
end
