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

require 'pedant/core_ext/net_http'

require 'pp' # Debugging

require 'rspec'
require 'rspec-shared'

require 'pedant/concern'
require 'pedant/json'
require 'pedant/requestor'
require 'pedant/request'
require 'pedant/platform'
require 'pedant/config'
require 'pedant/utility'
require 'pedant/sandbox'
require 'pedant/chef_utility'
require 'pedant/command_line'
require 'pedant/gem'
require 'pedant/knife'
require 'pedant/ui'

require 'pedant/rspec/matchers'
require 'pedant/rspec/common'

require 'veil'

module Pedant
  def self.config
    # This is a bit of a hack: for some reason, we have UTF-8/US-ASCII encoding
    # conversion issues when running against OHC (though not in dev-vm -- maybe it's
    # being forced some other way there?).  This shouldn't have any other effect on
    # the tests, but if we have a better solution at some point, we may be able to
    # remove the next two lines here.
    Encoding.default_external = Encoding::UTF_8
    Encoding.default_internal = Encoding::UTF_8

    Config
  end

  def self.setup(argv=[], option_sets=["core_options", "api_options"])
    config.from_argv(argv, option_sets)
    puts "Configuring logging..."
    configure_logging
    puts "Creating platform..."
    create_platform
    config.pedant_platform.before_configure_rspec

    puts "Starting Pedant Run: #{config.pedant_platform.pedant_run_timestamp}"
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

  def self.create_platform
    # Let's not expose the secrets store by default

    # TODO 2017-02-28 mp:  configurable location:
    path = ENV['SECRETS_FILE'] || "/etc/opscode/private-chef-secrets.json"
    credentials = Veil::CredentialCollection::ChefSecretsFile.from_file(path)

    config.pedant_platform = Pedant::Platform.new(config.chef_server,
                                                  credentials.get('chef-server', 'superuser_key'),
                                                  config.superuser_name)
  end

  def self.configure_rspec
    ::RSpec.configure do |c|
      c.expect_with :rspec do |expectation|
        expectation.syntax = [:should, :expect]
      end

      c.mock_with :rspec do |mock|
        mock.syntax = [:should, :expect]
      end

      # If you just want to run one (or a few) tests in development,
      # add :focus metadata
      c.filter_run :focus => true

      if Pedant.config.only_internal
        c.filter_run :cleanup
      else
        c.filter_run_excluding :cleanup => true unless Pedant.config.include_internal
      end

      if Pedant.config.only_internal_orgs
        c.filter_run :internal_orgs
      elsif Pedant.config.exclude_internal_orgs
        c.filter_run_excluding :internal_orgs => true
      end

      c.run_all_when_everything_filtered = true

      # This needs to be included everywhere
      c.include Pedant::RSpec::Common

      platform = Pedant::Config.pedant_platform

      platform.configure_rspec

      c.before(:suite) do
        platform.setup
      end

      c.after(:suite) do
        platform.cleanup
        print "\a" if Pedant.config.bell_on_completion
      end

    end
  end
end
