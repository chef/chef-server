require 'chef/provider/lwrp_base'

class Chef
  class Provider
    class ElasticSearchIndex < Chef::Provider::LWRPBase
      use_inline_resources if defined?(:use_inlined_resources)

      action :create_or_update do
        if index_exists?
          if index_changed?
            # (ES2->ES5 Migration - Aug/2017)
            # In the current upgrade scenario
            # any changes we attempt to make to the mappings for ES5 compat
            # are rejected with a 400:
            #
            # Mapper for [X_CHEF_id_CHEF_X] conflicts with existing mapping in
            # other types:
            #    mapper [X_CHEF_id_CHEF_X] has different [index] values
            #    mapper [X_CHEF_id_CHEF_X] has different [doc_values] values
            #    mapper [X_CHEF_id_CHEF_X] has different [analyzer]"
            #
            # Because a reindex is required after updating the index anyway,
            # we will recreate the index and rely on partybus migrations
            # to repopulate it when appropriate.
            #
            # Future updates to the index - if any - that would not require a
            # reindex make exploring incremental index/mapping updates
            # incremental updates worthwhile.
            #
            # The function converge_block_for_update was left in to show the
            # PUT request used for mapping updates, but it is not currently referenced.
            # TODO: This will be removed and left in git history in a separate PR.
            converge_block_for_destroy
            converge_block_for_create
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
          index = JSON.parse(solr_server.get(new_resource.index_name))
          @orig_mappings = index[new_resource.index_name]['mappings']
        end
        return @orig_mappings
      rescue Net::HTTPServerException => e
        if e.response && e.response.body
          Chef::Log.fatal("Failed to create index #{new_resource.index_name}: #{e.response.body}")
        end
        raise unless e.response && e.response.code == '404'
        return nil
      end

      def converge_block_for_create
        converge_by "Creating elasticsearch index #{new_resource.index_name}" do
          ::File.open("/vagrant/testdata/data.json", "w"){ |f| f.write(JSON.pretty_generate(new_resource.index_definition)) }
          solr_server.put(new_resource.index_name,
                          Chef::JSONCompat.to_json(new_resource.index_definition))
        end
      end

      def converge_block_for_destroy
        converge_by "Deleting elasticsearch index #{new_resource.index_name}" do
          begin
            solr_server.delete(new_resource.index_name)
          rescue Net::HTTPServerException => e
            if e.response && e.response.body
              Chef::Log.fatal("Failed to delete index #{new_resource.index_name}: #{e.response.body}")
            end
            raise unless e.response && e.response.code == '404'
          end
        end
      end

      def converge_block_for_update
        converge_by "Updating elasticsearch index #{new_resource.index_name}" do
          new_resource.index_definition['mappings'].each do |name, mapping|
            begin
              url = "#{new_resource.index_name}/_mapping/#{name}"
              solr_server.put("#{url}", #?update_all_types",
                              Chef::JSONCompat.to_json(mapping))
            rescue Net::HTTPServerException => e
              if e.response && e.response.body
                Chef::Log.fatal("Failed to update index #{new_resource.index_name}: #{e.response.body}")
              end
              raise
            end
          end
        end
      end

      def index_changed?
        new_index_mappings = new_resource.index_definition['mappings']
        field_maps_are_equal?(existing_index_mappings, new_index_mappings)
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
            return field_maps_are_equal?(v, v2)
          else
            # Other values back are limited to directly comparaible key-values
            return v == v2
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
