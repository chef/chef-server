require 'pedant/multitenant/response_bodies'
require 'pedant/multitenant/org_info'

# Adds RSpec configuration powers to the MultiTenantPlatform
module Pedant
  class MultiTenantPlatform
    def configure_rspec
      ::RSpec.configure do |c|
        puts "Configuring RSpec for Multi-Tenant Tests"
        c.run_all_when_everything_filtered = true
        c.filter_run_excluding :platform => :open_source

        c.include Pedant::MultiTenant::ResponseBodies
        c.include Pedant::MultiTenant::OrgInfo
      end
    end
  end
end
