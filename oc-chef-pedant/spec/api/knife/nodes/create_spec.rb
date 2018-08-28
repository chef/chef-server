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
    context 'create' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Node

      let(:command) { "knife node create #{node_name} -c #{knife_config} --disable-editing" }
      after(:each)  { knife "node delete #{node_name} -c #{knife_config} --yes" }

      context 'without existing node of the same name' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should succeed' do
            should have_outcome :status => 0, :stdout => /Created node\[#{node_name}\]/
          end
        end
      end

      context 'with an existing node of the same name' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should fail' do
            pending 'CHEF-982: `knife node create` does not report name conflicts'
            # Create a node with the same name
            #knife "node create #{node_name} -c #{knife_config} --disable-editing"
            post(api_url("/nodes"), platform.admin_user, payload: { "name" => node_name })

            # Run knife a second time
            should have_outcome :status => 0, :stderr => /Node #{node_name} already exists/
          end
        end
      end

    end
  end
end
