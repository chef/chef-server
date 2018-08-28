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
  context 'data bag' do
    context 'show [ITEM]' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::DataBag

      let(:command) { "knife data bag list -c #{knife_config}" }
      after(:each)  { knife "data bag delete #{bag_name} -c #{knife_config} --yes" }

      context 'as an admin' do
        let(:requestor) { knife_admin }

        it 'should succeed' do
          knife "data bag create #{bag_name} -c #{knife_config}"

          # Runs knife data bag list
          should have_outcome :status => 0, :stdout => /#{bag_name}/
        end
      end

    end
  end
end
