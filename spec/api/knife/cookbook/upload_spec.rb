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


describe 'knife', knife: true, skip: !open_source? do
  context 'cookbook' do
    context 'upload' do
      include Pedant::RSpec::KnifeUtil

      let(:command) { "knife cookbook upload #{cookbook_name} -c #{knife_config}" }
      let(:cookbook_name){ "joy_of_cooking" }
      let(:cwd) { repository }

      after(:each) { knife "cookbook delete #{cookbook_name} -c #{knife_config} --yes" }

      context 'as an admin' do
        let(:requestor) { knife_admin }

        it 'should succeed', :slow => !open_source? do
          should have_outcome :status => 0, :stdout => /Uploaded 1 cookbook/
        end
      end

      # Only admin clients can upload cookbooks on Open Source Chef
      context 'as a normal client', :platform => :open_source do
        let(:requestor) { knife_user }

        it 'should fail', :authorization do
          should have_outcome :status => 100, :stdout => /You are not allowed to take this action/
        end
      end

    end
  end
end
