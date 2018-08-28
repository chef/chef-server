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
    context 'bulk delete REGEX' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Node

      let(:command) { "knife node bulk delete '^pedant-node-' -c #{knife_config} --yes" }
      let(:nodes)   { %w(pedant-node-0 pedant-node-1 pedant-master) }
      after(:each)  { nodes.each(&delete_node!) }

      let(:create_node!) { ->(n) { knife "node create #{n} -c #{knife_config} --disable-editing" } }
      let(:delete_node!) { ->(n) { knife "node delete #{n} -c #{knife_config} --yes" } }

      context 'as an admin' do
        let(:requestor) { knife_admin }

        it 'should succeed' do
          nodes.each(&create_node!)

          # Runs knife node list
          should have_outcome :status => 0, :stdout => /Deleted node pedant-node-0\s+Deleted node pedant-node-1/
        end
      end

    end
  end
end
