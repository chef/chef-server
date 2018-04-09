require 'chef/resource/lwrp_base'

class Chef
  class Resource
    class ChefRun < Chef::Resource::LWRPBase
      provides :chef_run
      resource_name :chef_run

      actions :run
      default_action :run

      attribute :run_list, :kind_of => String, :name_attribute => true
      attribute :cookbook_path
      attribute :included_attrs, :kind_of => Array, :default => []
      attribute :show_run, :kind_of => [TrueClass, FalseClass], :default => true
    end
  end
end
