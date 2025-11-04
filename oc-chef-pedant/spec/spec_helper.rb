# spec_helper.rb for oc-chef-pedant
# This allows running individual spec files with `bundle exec rspec spec/some_spec.rb`
# without needing the full bin/oc-chef-pedant setup

require "bundler/setup"
require "rspec"
require "pedant"

# Minimal Pedant configuration for local testing
# Note: Most tests will fail without a running Chef Server, but this allows
# verifying that the code loads without ActiveSupport errors

module Pedant
  module TestConfig
    def self.setup_minimal_config
      # Set minimal required config to avoid crashes during load
      Pedant.config.suite = %w{api}
      
      # Disable logging to avoid file system errors on Windows
      Pedant.config.log_file = nil
      
      # Create mock platform to avoid needing a real Chef Server
      mock_platform = Object.new
      def mock_platform.before_configure_rspec; end
      def mock_platform.configure_rspec; end
      def mock_platform.setup; end
      def mock_platform.cleanup; end
      def mock_platform.pedant_run_timestamp; Time.now.to_i; end
      def mock_platform.stats_password; 'mock_password'; end
      def mock_platform.webui_key; nil; end  # OSS specs check for this
      
      # Set the platform before configure_rspec is called
      Pedant.config.define_singleton_method(:pedant_platform) { mock_platform }
      
      # Configure RSpec with Pedant's settings
      Pedant.configure_rspec
    end
  end
end

# Initialize Pedant for testing
Pedant::TestConfig.setup_minimal_config

puts "Pedant test environment initialized (minimal mode)"
puts "Note: Tests requiring a running Chef Server will fail, but you can verify code loads correctly"
