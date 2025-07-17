# RSpecShared, written by Justin Ko <jko170@gmail.com>
# Original at https://github.com/justinko/rspec-shared

require 'rspec/core'
require 'rspec-shared/methods'
RSpec.configuration.extend RSpecShared::Methods

# Make shared method available in example groups
RSpec.configure do |config|
  config.extend RSpecShared::Methods
end
