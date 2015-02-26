# Copyright: Copyright (c) 2012 Opscode, Inc.
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

describe 'knife', knife: true, skip: !open_source? do
  context 'node' do
    context 'from file NODE', skip: "OC-3957: Write specs for `knife node from file`" do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::KnifeUtil::Node

      let(:command) { "knife node from file #{node_name}.json -c #{knife_config}" }
      after(:each) { knife "node delete #{node_name} -c #{knife_config} --yes" }

      context 'with existing node' do
        context 'as an admin' do
          let(:requestor) { knife_admin }

          it 'should succeed' do #, skip: "ERROR: Method not allowed when using node from file?" do
            assume_fixture_file!

            # Runs knife node from file
            should have_outcome :status => 0, :stdout => /Node Name:\s+#{node_name}/
          end

          skip 'should have attributes pulled in from file'
        end
      end

    end
  end
end
