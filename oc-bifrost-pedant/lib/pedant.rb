# Copyright: Copyright (c) 2012 Opscode, Inc.
# License: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require 'pp' # Debugging

require 'rspec'
require 'rspec-shared'

require 'pedant/concern'

require 'pedant/config'
require 'pedant/utility'

require 'pedant/command_line'
require 'pedant/ui'

require 'pedant/rspec/matchers'
require 'pedant/rspec/common'

module Pedant
  def self.config
    Config
  end

  def self.setup(argv=[])
    config.from_argv(argv)
    puts "Configuring logging..."
    configure_logging
    configure_rspec
  end

  # Enable detailed HTTP traffic logging for debugging purposes
  def self.configure_logging
    if config.log_file
      require 'net-http-spy'
      Net::HTTP.http_logger_options = {
        :trace =>true,
        :verbose => true,
        :body => true
      }
      Net::HTTP.http_logger = Logger.new(config.log_file)
    end
  end

  def self.configure_rspec
    ::RSpec.configure do |c|
      c.treat_symbols_as_metadata_keys_with_true_values = true
      c.filter_run :focus => true
      c.run_all_when_everything_filtered = true

      # This needs to be included everywhere
      c.include Pedant::RSpec::Common

      # platform = Pedant::Config.pedant_platform

      # if platform.respond_to?(:configure_rspec)
      #   puts "setting up rspec config for #{platform}"
      #   platform.configure_rspec
      # end

      c.before(:suite) do
#        platform.setup
      end

      c.after(:suite) do
 #       platform.cleanup
        print "\a" if Pedant.config.bell_on_completion
      end

    end
  end
end
