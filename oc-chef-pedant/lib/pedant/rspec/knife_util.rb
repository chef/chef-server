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

require 'mixlib/shellout'
require 'pathname'

module Pedant
  module RSpec
    module KnifeUtil
      extend Pedant::Concern

      included do
        subject { knife_run }
        let(:knife_run) { run command }
        let(:command)   { fail 'Define let(:command) in the spec' }

        # Default cwd for executing knife commands
        let(:cwd) { temp_repository }

        # Our test repository for all knife commands.  Note that this is
        # relative to the top-level opscode-pedant directory.
        let(:repository) { Pedant::Utility.fixture_path "test_repository" }

        # The temporary test repository for randomly generated data
        let(:temp_repository) { requestor.knife_dir }

        let(:assume_fixture_file!) do
          File.open(fixture_file_path, 'w') do |f|
            f.write(fixture_file_content)
          end
        end

        let(:with_random_key_and_value) { ->(h, i) { h.with(SecureRandom.uuid, SecureRandom.base64(rand(50) + 100)) } }
        let(:random_hash) { ->(m) { (1..m).to_a.inject({}, &with_random_key_and_value) } }
        let(:random_array) { ->(m) { (1..m).map { SecureRandom.uuid } } }

        let(:random_max)     { ->() { rand(7) + 3 } }

        # Override
        let(:fixture_file_path)    { fail "Define :fixture_file_path" }
        let(:fixture_file_content) { fail "Define :fixture_file_content" }

        # The knife config file that everyone uses.  It is relative to
        # +repository+ (see above).
        #
        # TODO: In the future, have a knife config for each of multiple users
        let(:knife_config) { requestor.knife_rb_path }
        let(:knife_config_for_normal_user) { knife_user.knife_rb_path}
        let(:knife_config_for_admin_user)  { knife_admin.knife_rb_path }

        # Override let(:requestor) for a different directory.
        # If the requestor does not have :create_knife set to true, then this
        # will fail.
        def knife_fixture(fixture)
          _path = Pathname.new(requestor.knife_dir).join(fixture)
          return _path if File.exists?(_path)
          Dir.mkdir(_path)
          _path
        end

        # Convenience method for creating a Mixlib::ShellOut representation
        # of a knife command in our test repository
        def shell_out(command_line)
          Mixlib::ShellOut.new(command_line,
                                 'cwd' => cwd,
                                 'env' => command_environment)
        end

        # When running pedant in a standalone configuration, Bundler will
        # prevent `knife` from running properly because pedant doesn't have a
        # dependency on Chef. However, unsetting all the ruby/bundler
        # environment variables will prevent these tests from working correctly
        # when run from a bundle that includes chef and is running under RVM.
        # Set the `PEDANT_ALLOW_RVM` environment variable (to anything) to opt
        # out of the bundle/rvm busting mode.
        def command_environment
          if ENV["PEDANT_ALLOW_RVM"]
            {}
          else
            {
              'BUNDLE_GEMFILE' => nil,
              'BUNDLE_BIN_PATH' => nil,
              'GEM_PATH' => nil,
              'GEM_HOME' => nil,
              'RUBYOPT' => nil
            }
          end
        end

        # Convenience method for actually running a knife command in our
        # testing repository.  Returns the Mixlib::Shellout object ready for
        # inspection.
        def run(command_line)
          shell_out(command_line).tap(&:run_command)
        end

        def run_debug(command_line)
          puts "CWD: #{cwd}"
          shell_out(command_line.tap(&watch)).tap(&:run_command).tap { |x| puts "Status: #{x.status} #{x.stdout} #{x.stderr}" }
        end

        def knife(knife_command)
          run "knife #{knife_command}"
        end

        def knife_debug(knife_command)
          run_debug "knife #{knife_command}"
        end

      end # included

      module DataBag
        extend Pedant::Concern

        included do
          let(:bag_name) { "pedant_#{rand(1000000)}" }
          let(:assume_data_bag_item_file!) { assume_fixture_file! }

          let(:fixture_file_path) { data_bag_item_file_path }
          let(:fixture_file_content) { item_file_content }

          let(:data_bag_item_file_path) { "#{data_bag_dir}/#{item_name}.json" }
          let(:data_bag_dir) { knife_fixture "data_bags" }

          let(:max_keys) { rand(7) + 3 }
          let(:item_name) { "item_#{rand(100000)}" }
          let(:item) { random_hash.(max_keys) }

          let(:item_file_content) { ::JSON.generate(item.with(:id, item_name)) }
        end
      end # DataBag

      module Node
        extend Pedant::Concern

        included do
          let(:node_name) { "pedant_#{rand(1000000)}" }

          let(:fixture_file_path) { "#{node_data_dir}/#{node_name}.json" }
          let(:fixture_file_content) { ::JSON.generate(node_data) }
          let(:node_data_dir) { knife_fixture "nodes" }

          let(:node_data) do
            {
              'name' => node_name,
              'node_name' => node_name,
              'override' => node_override,
              'normal' => node_normal,
              'default' => node_default,
              'automatic' => node_automatic,
              'run_list' => node_run_list
            }
          end

          let(:node_override)  { random_hash.(random_max.()) }
          let(:node_normal)    { random_hash.(random_max.()) }
          let(:node_default)   { random_hash.(random_max.()) }
          let(:node_automatic) { random_hash.(random_max.()) }
          let(:node_run_list)  { random_array.(random_max.()) }
        end
      end

      module Role
        extend Pedant::Concern

        included do
          let(:role_name) { "pedant_#{rand(1000000)}" }
          let(:role_description) { SecureRandom.uuid }

          let(:fixture_file_path) { "#{role_data_dir}/#{role_name}.json" }
          let(:fixture_file_content) { ::JSON.generate(role_data) }
          let(:role_data_dir) { knife_fixture "roles" }

          let(:role_data) do
            {
              'name' => role_name,
              'description' => role_description,
              'default_attributes' => role_default,
              'override_attributes' => role_override,
              'run_list' => role_run_list,
            }
          end

          let(:role_override)  { random_hash.(random_max.()) }
          let(:role_default)   { random_hash.(random_max.()) }
          let(:role_run_list)  { random_array.(random_max.()) }

          # TODO: Make fake env_run_list data
          #let(:role_env_run_lists)  { random_array.(random_max.()) }
        end
      end # Role
    end # KnifeUtil
  end # RSpec
end # Pedant
