require 'chef/resource/lwrp_base'

class Chef
  class Resource
    class ElasticSearchIndex < Chef::Resource::LWRPBase
      provides :elasticsearch_index
      resource_name :elasticsearch_index
      default_action :create
      attribute :index_name, name_attribute: true
      attribute :server_url, kind_of: String
      attribute :index_definition, kind_of: Hash
    end
  end
end
