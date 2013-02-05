# RSpecShared, written by Justin Ko <jko170@gmail.com>
# Original at https://github.com/justinko/rspec-shared

require 'rspec/core'
require 'rspec-shared/methods'
RSpec.configuration.extend RSpecShared::Methods
