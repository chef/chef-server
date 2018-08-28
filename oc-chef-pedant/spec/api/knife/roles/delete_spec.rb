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
  context 'role' do
    context 'delete [ROLE]' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Role

      let(:command) { "knife role delete #{role_name} -c #{knife_config} --yes" }
      after(:each) { knife "role delete #{role_name} -c #{knife_config} --yes" }

      context 'without existing role' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should fail' do
            should have_outcome :status => 100,
              :stderr => /Cannot load role #{role_name}/,
              :stderr => /The object you are looking for could not be found/
          end
        end
      end

      context 'with existing role' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should succeed' do
            # Create a role with the same name
            knife "role create #{role_name} -c #{knife_config} -d #{role_description}"

            # Run knife a second time
            should have_outcome :status => 0, :stdout => /Deleted role\[#{role_name}\]/
          end
        end
      end

    end
  end
end
