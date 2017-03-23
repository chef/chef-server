ENV["RAILS_ENV"] ||= 'test'

def setup_chef_secrets
  f = File.open(File.expand_path("../../config/private-chef-secrets.json", __FILE__), "r")
  puts "Using development secrets file at FD #{f.to_i}"
  ENV['CHEF_SECRETS_FD'] = f.to_i.to_s
  f
end

#
# Holy cow what is this?
#
# Our secrets library expect to be passed credentials via a file
# descriptor. However, depending on how rspec was launched, someone
# may have already consumed the secrets and closed the file, so here
# we check if the passed FD is valid or create a new one.
#
f = if ENV['CHEF_SECRETS_FD'].nil?
      puts "No CHEF_SECRETS_FD set"
      setup_chef_secrets
    else
      begin
        puts "CHEF_SECRETS_FD is set to #{ENV['CHEF_SECRETS_FD']}"
        f = IO.new(ENV['CHEF_SECRETS_FD'].to_i, "r")
        if f.closed?
          puts "File is already closed. Likely running from rake."
          setup_chef_secrets
        else
          f
        end
      rescue Errno::EBADF, Errno::EINVAL
        puts "CHEF_SECRETS_FD has already been read. Likely running from rake."
        setup_chef_secrets
      end
    end

at_exit do
  # Ensure f stays open until the at_exit handler runs
  # Note: it will be closed by veil
  f
end

require File.expand_path("../../config/environment", __FILE__)
require 'rspec/rails'

Dir[Rails.root.join("spec/support/**/*.rb")].each { |f| require f }
ActiveRecord::Migration.maintain_test_schema!
ActiveRecord::Migration.check_pending! if defined?(ActiveRecord::Migration)

RSpec.configure do |config|
  config.expect_with :rspec do |rspec_configuration|
    rspec_configuration.syntax = :expect
  end

  config.infer_spec_type_from_file_location!
  config.fixture_path = "#{::Rails.root}/spec/fixtures"
  config.use_transactional_fixtures = true
  config.infer_base_class_for_anonymous_controllers = false
  config.order = "random"
  config.include Capybara::DSL

  config.before do
    ActionMailer::Base.deliveries.clear
  end
end
