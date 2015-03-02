require 'pedant/response_bodies'
require 'pedant/org_info'
require 'fileutils'

# Adds RSpec configuration powers to the Platform
module Pedant
  class Platform
    def configure_rspec
      # Create a path for all of our generated content. Makes it easier
      # to do a final cleanup pass
      FileUtils.mkpath File.join(Dir.tmpdir, "oc-chef-pedant")
      ::RSpec.configure do |c|
        c.run_all_when_everything_filtered = true
        c.filter_run_excluding :platform => :open_source, :intermittent_failure => true

        c.include Pedant::ResponseBodies
        c.include Pedant::OrgInfo
      end
    end
  end
end
