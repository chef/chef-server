ENV["RAILS_ENV"] ||= 'test'
ENV["CHEF_SECRETS_DATA"] ||= File.read(File.expand_path("../../config/private-chef-secrets.json", __FILE__))

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
