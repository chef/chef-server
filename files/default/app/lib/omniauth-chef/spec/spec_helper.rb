$:.unshift File.expand_path '..', __FILE__

$:.unshift File.expand_path '../../lib', __FILE__

require 'omniauth'
require 'omniauth-chef'

require 'rack/test'

require 'rspec'

RSpec.configure do |configuration|
  configuration.extend OmniAuth::Test::StrategyMacros, type: :strategy

  configuration.include Rack::Test::Methods

  configuration.expect_with :rspec do |rspec_configuration|
    rspec_configuration.syntax = :expect
  end
end
