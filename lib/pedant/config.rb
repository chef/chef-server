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

require 'mixlib/config'
require 'pedant/command_line'
require 'pedant/gem'

# RSpec ReRun requires this until some version after 0.2.0
require 'rspec/legacy_formatters'
require 'rspec-rerun/formatters/failures_formatter'

module Pedant
  class Config
    extend Mixlib::Config

    # Configure Pedant based on command-line arguments
    def self.from_argv(argv, option_sets)
      cli_options = Pedant::CommandLine.new(argv).parse(option_sets)
      cli_options.config_file ||= self.config_file

      if File.exist?(cli_options.config_file)
        self.from_file(cli_options.config_file)
      else
        raise "Configuration file '#{cli_options.config_file}' not found!"
      end

      # --tag TAG:VALUE  (inclusion filter)
      # --tag ~TAG:VALUE (exclusion filter)
      self[:tags] = cli_options.foci + cli_options.skip.map { |tag| "~#{tag}" }

      # ensure we remove any nil options before merging so we don't clobber
      # values set in the default Pedant::Config class OR user provided config
      # file
      non_nil_cli_options = cli_options.to_hash.delete_if{|key, value| value.nil? }
      merge!(non_nil_cli_options)
    end

    # Generate a list of directories in which Pedant will look for
    # tests.  Includes only the tests specified by the value of
    # +Pedant.config.suite+.
    def self.test_directories
      suite = Pedant.config.suite || raise("Test suite unspecified!  Set 'Pedant.config.suite' in the test runner!")
      Pedant::Gem.test_directories(suite)
    end


    # Return an array of arguments for RSpec.
    def self.rspec_args
      args = []

      # Only apply filtering flags if 'run_all' is not set to override them
      if self[:tags] && !run_all
        args.concat(self[:tags].map { |tag| ['-t', tag.to_s] } )
      end

      args.concat(rspec_formatting_args)

      # Load up the failures file if we're re-running
      if rerun
        args.concat(%W[-O #{::RSpec::Rerun::Formatters::FailuresFormatter::FILENAME}])
      else
        # Remove the failures file if we aren't running with --rerun;
        # otherwise, if it exists, we would only ever run those tests,
        # even if they all pass!
        FileUtils.rm(::RSpec::Rerun::Formatters::FailuresFormatter::FILENAME, :force => true)
      end

      _test_dirs = test_directories
      puts "Running tests from the following directories:", _test_dirs
      args.concat _test_dirs

      args.flatten
    end

    # Returns just the arguments for formatting
    def self.rspec_formatting_args
      format_args = if junit_file
                      %W[-r rspec_junit_formatter -f RspecJunitFormatter -o #{junit_file} -f documentation]
                    else
                      %w[ --color -f documentation --tty]
                    end

      # Always use the failures formatter, in case we want to rerun failures
      format_args.concat(%W[-f #{::RSpec::Rerun::Formatters::FailuresFormatter}])
    end

    # Default Values
    ################################################################################

    # Default to a config file in the current directory
    config_file "pedant_config.rb"

    # Maximum time in seconds that search endpoint requests should be
    # retried before giving up (to accommodate the asynchronous
    # commits of Solr)
    maximum_search_time 65

    # Amout of time to sleep (in seconds) after performing a direct
    # Solr query (rather than via the Chef API).  This is used, e.g.,
    # following an explicit Solr commit.  If you are getting
    # intermittent search-related failures, try bumping this up.
    direct_solr_query_sleep_time 0.500

    # HTTP logging is turned off by default
    log_file(false)

    # JUnit output is turned off by default
    junit_file(false)

    # Error message verification is on by default
    verify_error_messages(true)

    # Emits a console bell when run finishes
    bell_on_completion(false)

    # Default org mode is false by default
    use_default_org(false)

    # Default orgname is nil by default
    default_orgname(nil)
  end
end
