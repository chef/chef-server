require 'chef/resource/lwrp_base'

class Chef
  class Resource
    class ElasticSearchIndex < Chef::Resource::LWRPBase
      self.resource_name = 'elasticsearch_index'
      actions [:create_or_update, :destroy]
      default_action :create_or_update
      attribute :index_name, :name_attribute => true
      attribute :server_url, :kind_of => String
      attribute :index_definition, :kind_of => Hash
    end
  end
end
