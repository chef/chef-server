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

      let(:command) { "knife data bag show #{bag_name} #{item_name} -c #{knife_config}" }

      let(:assume_existing_data_bag!) do
        assume_data_bag_item_file!
        knife "data bag create #{bag_name} -c #{knife_config}"
        knife "data bag from file #{bag_name} #{data_bag_item_file_path} -c #{knife_config}"
      end

      after(:each) { knife "data bag delete #{bag_name} -c #{knife_config} --yes" }

      context 'with existing data bag' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should succeed' do
            assume_existing_data_bag!

            # Runs knife data bag from file
            should have_outcome :status => 0, :stdout => /id:\s+#{item_name}/
          end

          context 'without existing item name' do
            it 'should fail' do
              # Create the data bag but not the item
              knife "data bag create #{bag_name} -c #{knife_config}"

              # Runs knife data bag from file
              should have_outcome :status => 100, :stderr => /Cannot load data bag item #{item_name} for data bag #{bag_name}/
            end
          end
        end
      end

      context 'without existing data bag' do
        let(:bag_name) { "does_not_exist_#{rand(1000)}" }

        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should fail' do
            should have_outcome :status => 100, :stderr => /Cannot load data bag item #{item_name} for data bag #{bag_name}/
          end
        end
      end

    end
  end
end
