# Copyright: Copyright 2012-2018 Chef Software, Inc.
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

require 'pedant/rspec/knife_util'
require 'securerandom'

describe 'knife', :knife do
  context 'node' do
    context 'run_list' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Node

      let(:knife_show) { knife "node show #{node_name} -c #{knife_config}" }

      let(:assume_existing_node!) do
        assume_fixture_file!
        knife "node create #{node_name} -c #{knife_config} --disable-editing"
        #knife_debug "node from file #{fixture_file_path} -c #{knife_config}"
        # This command does not actually w work for some reason, with knife reporting
        # "Method not allowed"
        # knife "node from file #{node_name}.json -c #{knife_config}"
      end

      after(:each) { knife "node delete #{node_name} -c #{knife_config} --yes" }
      let(:run_list) { "app,web" }
      let(:expected_run_list) { /recipe\[app\]\s+recipe\[web\]/ }

      context 'add NODE [ENTRIES,ENTRIES...]' do
        let(:command) { "knife node run_list add #{node_name} #{run_list} -c #{knife_config}" }

        context 'with existing node' do
          context 'as an admin' do
            let(:requestor) { knife_admin }

            it 'should succeed' do
              assume_existing_node!

              # Adds run_list item
              should have_outcome :status => 0, :stdout => /run_list:\s+#{expected_run_list}/
            end
          end
        end

        context 'without existing node' do
          let(:node_name) { "does_not_exist_#{rand(1000)}" }

          context 'as an admin' do
            let(:requestor) { knife_admin }

            it 'should fail' do
              should have_outcome :status => 100, :stderr => /node '#{node_name}' not found/
            end
          end
        end
      end

      context 'remove NODE [ENTRIES,ENTRIES...]' do
        let(:command) { "knife node run_list remove #{node_name} recipe[app] -c #{knife_config}" }

        context 'with existing node' do
          context 'as an admin' do
            let(:requestor) { knife_admin }

            it 'should succeed' do
              assume_existing_node!
              knife "node run_list add #{node_name} app,web -c #{knife_config}"

              # Adds run_list item (regex has optional [] surrounding the recipe for Chef 10/11 difference in output)
              should have_outcome :status => 0, :stdout => /run_list:\s+\[?recipe\[web\]\]?/
            end
          end
        end

        context 'without existing node' do
          let(:node_name) { "does_not_exist_#{rand(1000)}" }

          context 'as an admin' do
            let(:requestor) { knife_admin }

            it 'should fail' do
              should have_outcome :status => 100, :stderr => /node '#{node_name}' not found/
            end
          end
        end
      end

    end
  end
end
