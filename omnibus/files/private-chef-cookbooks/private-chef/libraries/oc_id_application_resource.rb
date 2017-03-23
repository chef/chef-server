require 'chef/resource/lwrp_base'

class Chef
  class Resource
    class OcIdApplication < Chef::Resource::LWRPBase
      self.resource_name = 'oc_id_application'

      actions :create
      default_action :create

      attribute :name, :kind_of => String, :name_attribute => true
      attribute :write_to_disk, :kind_of => [TrueClass, FalseClass], :default => false
      attribute :redirect_uri, :kind_of => String, :required => true
    end
  end
end
