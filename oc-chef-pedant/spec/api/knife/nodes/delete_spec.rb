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

describe 'knife', :knife do
  context 'node' do
    context 'delete' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Node

      let(:command) { "knife node delete #{node_name} -c #{knife_config} --yes" }
      after(:each) { knife "node delete #{node_name} -c #{knife_config} --yes" }

      context 'without existing node' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should fail' do
            should have_outcome :status => 100,
              :stderr => /node '#{node_name}' not found/,
              :stderr => /The object you are looking for could not be found/
          end
        end
      end

      context 'with existing node' do
        context 'as an admin' do
          let(:command) { "knife node delete #{node_name} -c #{knife_config} --yes" }
          let(:requestor) { knife_admin }
          let(:knife_run) { run command }

          it 'should succeed' do
            # Create a node with the same name
            knife "node create #{node_name} -c #{knife_config} --disable-editing"

            # Run knife a second time
            should have_outcome :status => 0, :stdout => /Deleted node\[#{node_name}\]/
          end
        end
      end

    end
  end
end
