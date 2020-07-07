require 'chef/resource/lwrp_base'

class Chef
  class Resource
    class OcIdApplication < Chef::Resource::LWRPBase
      provides :oc_id_application
      resource_name :oc_id_application
      default_action :create

      attribute :write_to_disk, kind_of: [TrueClass, FalseClass], default: false
      attribute :redirect_uri, kind_of: String, required: true
    end
  end
end
