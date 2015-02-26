require 'pedant/multitenant/response_bodies'
require 'pedant/multitenant/org_info'
require 'fileutils'

# Adds RSpec configuration powers to the MultiTenantPlatform
module Pedant
  class MultiTenantPlatform
    def configure_rspec
      # Create a path for all of our generated content. Makes it easier
      # to do a final cleanup pass
      FileUtils.mkpath File.join(Dir.tmpdir, "oc-chef-pedant")
      ::RSpec.configure do |c|
        puts "****** Configuring RSpec for Multi-Tenant Tests"
        c.run_all_when_everything_filtered = true
        c.filter_run_excluding :platform => :open_source, :intermittent_failure => true

        c.include Pedant::MultiTenant::ResponseBodies
        c.include Pedant::MultiTenant::OrgInfo
      end
    end
  end
end
