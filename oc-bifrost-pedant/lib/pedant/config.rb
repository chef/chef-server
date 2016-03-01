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

require 'rspec-rerun/formatters/failures_formatter'

module Pedant
  class Config
    extend Mixlib::Config

    # Configure Pedant based on command-line arguments
    def self.from_argv(argv)
      cli_options = Pedant::CommandLine.new(argv).parse()
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

    # Return an array of arguments for RSpec.
    def self.rspec_args
      args = []

      # Only apply filtering flags if 'run_all' is not set to override them
      if self[:tags] && !run_all
        args.concat(self[:tags].map { |tag| ['-t', tag.to_s] } )
      end

      if junit_file
        args.concat(%W[-r rspec_junit_formatter -f RspecJunitFormatter -o #{junit_file} -f documentation])
      else
        args.concat(%w[ --color -f documentation ])
      end

      # Always use the failures formatter, in case we want to rerun failures
      args.concat(%W[-f #{::RSpec::Rerun::Formatters::FailuresFormatter}])

      # Load up the failures file if we're re-running
      if rerun
        args.concat(%W[-O #{::RSpec::Rerun::Formatters::FailuresFormatter::FILENAME}])
      else
        # Remove the failures file if we aren't running with --rerun;
        # otherwise, if it exists, we would only ever run those tests,
        # even if they all pass!
        FileUtils.rm(::RSpec::Rerun::Formatters::FailuresFormatter::FILENAME, :force => true)
      end

      # Tell it which tests to run
      args.concat ["spec/bifrost"]

      args.flatten
    end

    # Default Values
    ################################################################################

    # Default to a config file in the current directory
    config_file "pedant_config.rb"

    # HTTP logging is turned off by default
    log_file(false)

    # JUnit output is turned off by default
    junit_file(false)

    # Error message verification is on by default
    verify_error_messages(true)

    # Emits a console bell when run finishes
    bell_on_completion(false)
  end
end
