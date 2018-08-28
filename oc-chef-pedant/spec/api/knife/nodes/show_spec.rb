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
    context 'show [NODE]' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Node

      let(:command) { "knife node show #{node_name} -c #{knife_config}" }

      let(:assume_existing_node!) do
        assume_fixture_file!
        knife "node create #{node_name} -c #{knife_config} --disable-editing"
        #knife_debug "node from file #{fixture_file_path} -c #{knife_config}"
        # This command does not actually w work for some reason, with knife reporting
        # "Method not allowed"
        # knife "node from file #{node_name}.json -c #{knife_config}"
      end

      after(:each) { knife "node delete #{node_name} -c #{knife_config} --yes" }

      context 'with existing node' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should succeed' do
            assume_existing_node!

            # Runs knife node from file
            should have_outcome :status => 0, :stdout => /Node Name:\s+#{node_name}/
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
