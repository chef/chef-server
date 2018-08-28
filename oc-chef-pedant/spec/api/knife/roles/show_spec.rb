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
  context 'role' do
    context 'show [ROLE]' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Role

      let(:command) { "knife role show #{role_name} -c #{knife_config}" }
      let(:assume_existing_role!) { knife "role create #{role_name} -c #{knife_config} -d '#{role_description}'" }

      after(:each) { knife "role delete #{role_name} -c #{knife_config} --yes" }

      context 'with existing role' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should succeed' do
            assume_existing_role!

            # Runs knife role from file
            should have_outcome :status => 0, :stdout => /name:\s+#{role_name}/
          end
        end
      end

      context 'without existing role' do
        let(:role_name) { "does_not_exist_#{rand(1000)}" }

        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should fail' do
            should have_outcome :status => 100, :stderr => /The object you are looking for could not be found/
          end
        end
      end

    end
  end
end
