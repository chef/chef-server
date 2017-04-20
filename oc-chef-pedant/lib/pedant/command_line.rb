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

require 'optparse'

module Pedant

  class CommandLine < Struct.new(:junit_file, :config_file, :log_file, :include_internal, :only_internal,
                                 :run_all, :exclude_internal_orgs, :only_internal_orgs, :verify_error_messages,
                                 :bell_on_completion, :rerun, :seed, :use_default_org, :ssl_version, :server_api_version)

    def initialize(argv)
      @argv = argv.dup
    end

    def parse(option_sets)
      parser(option_sets).parse(@argv)
      self
    end

    def foci
      @foci ||= []
    end

    def skip
      @skip ||= []
    end

    def core_options(opts)
      opts.on("-J", "--junit-file FILE", "write JUnit output to FILE") do |j|
        self.junit_file = j
      end

      opts.on("-c", "--config FILE", "read configuration from FILE") do |f|
        self.config_file = f
      end

      opts.on("-L", "--log-file FILE", "Log HTTP communication to FILE") do |f|
        self.log_file = f
      end

      opts.on("-I", "--[no-]only-internal", "Only run Chef internal tests (no API tests)") do |i|
        self.only_internal = i
      end

      opts.on("-i", "--[no-]include-internal", "Run Chef internal tests in addition to API tests") do |i|
        self.include_internal = i
      end

      opts.on("-O", "--only-internal-orgs", "Only run Chef internal organization tests (no API tests)") do |i|
        self.only_internal_orgs = i
      end

      opts.on("-o", "--exclude-internal-orgs", "Exclude Chef internal organization tests from API tests (included by default)") do |i|
        self.exclude_internal_orgs = i
      end

      opts.on("-h", "--help", "Print this help message") do
        puts opts
        exit 1
      end

      opts.on("--use-default-org", "Use Pedant in Default Orgname mode. default_orgname must be configured in config file") do
        self.use_default_org = true
      end

      opts.on("--[no-]verify-error-messages", "Whether to verify error messages (on by default)") do |verify_error_messages|
        self.verify_error_messages = verify_error_messages
      end

      opts.on("--focus", "--focus TAGS,TAGS", "Focus on these tests") do |f|
        self.foci.concat f.split(/,/)
      end

      opts.on("--skip", "--skip TAGS,TAGS", "Skip these tests") do |f|
        self.skip.concat f.split(/,/)
      end

      opts.on("--skip-pedantic", "Skip pedantic tests") do
        self.skip << :pedantic
      end

      opts.on("--skip-slow", "Skip slow tests") do
        self.skip << :slow
      end

      opts.on("--smoke", "Run smoke tests (quick, cursory testing; safe for running on an existing system)") do
        self.foci << :smoke
      end

      opts.on("--all", "Run all tests.  Supersedes any other filtering-related arguments") do
        self.run_all = true
      end

      opts.on("--bell", "Emits a console bell after completing tests") do
        self.bell_on_completion = true
      end

      opts.on("--rerun", "Run tests that failed the last time") do
        self.rerun = true
      end

      opts.on("--seed SEED", "Use SEED for random ordering") do |seed|
        self.seed = seed
      end

      opts.on("--ssl-version VERSION", "Specify SSL version to use when connecting to an ssl-enabled endpoint. Defaults to TLSv1 if not specified") do |v|
        self.ssl_version = f.split(/ /).first.to_sym
      end

      opts.on("-V", "--server-api-version VERSION", "Set the Server API version to use in requests to the server") do |v|
        self.server_api_version = v
      end
    end

    def export_options(opts, tags)
      sorted = tags.sort

      # --help does not actually sort these, so ordering is important.
      sorted.each do |tag|
        opts.on("--focus-#{tag}", "Run only #{tag} tests") do
          self.foci << tag.gsub("-", "_") # allow hyphenated tags that resolve to symbols/tags with underscores
        end
      end

      sorted.each do |tag|
        opts.on("--skip-#{tag}", "Skip #{tag} tests") do
          self.skip << tag.gsub("-", "_")
        end
      end

    end

    def api_options(opts)
      tags = %w(environments cookbooks data_bags nodes roles sandboxes users
                clients depsolver search knife validation authentication authorization
                principals acl containers groups association omnibus organizations
                usags controls keys cookbook-artifacts license headers server-api-version
                policies pedantic self-test api-v0 api-v1 object-identifiers
                multiuser universe chef-zero-quirks user-keys client-keys required-recipe)
      export_options(opts, tags)
    end

    def parser(option_sets)
      @parser ||= OptionParser.new do |opts|
        opts.banner = "Usage: opscode-pedant [options]"
        option_sets.each do |option_set|
          self.send(option_set, opts)
        end
      end
    end

    def to_hash
      members.inject({}) do |as_hash, option|
        as_hash[option] = send(option)
        as_hash
      end
    end

  end
end
