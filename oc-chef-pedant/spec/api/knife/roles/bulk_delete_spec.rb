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

# Bulk role deletion currently uses search under the hood; we're
# including the search_util to access the 'force_solr_commit' helper
# method
require 'pedant/rspec/search_util'

describe 'knife', :knife do
  context 'role' do
    context 'bulk delete REGEX' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Role
      include Pedant::RSpec::SearchUtil

      let(:command) { "knife role bulk delete '^pedant-role-' -c #{knife_config} --yes" }
      let(:roles)   { %w(pedant-role-0 pedant-role-1 pedant-master) }
      after(:each)  { roles.each(&delete_role!) }

      let(:create_role!) { ->(n) { knife "role create #{n} -c #{knife_config} -d #{role_description}" } }
      let(:delete_role!) { ->(n) { knife "role delete #{n} -c #{knife_config} --yes" } }

      context 'as an admin' do
        let(:requestor) { knife_admin }

        it 'should succeed' do
          # Create all the testing roles
          roles.each(&create_role!)

          # Since bulk delete uses search, and search is eventually consistent,
          # we need to wait until the search results are ready.
          with_search_polling do
            # Runs knife role list
            should have_outcome :status => 0, :stdout => /Deleted role pedant-role-0\s+Deleted role pedant-role-1/
          end
        end
      end

    end
  end
end
