# Copyright: Copyright 2016-2018 Chef Software, Inc.
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
require 'pedant/rspec/user_util'

describe 'knife', :validation, :knife do
  context 'opc' do
    context 'org' do
      context 'create' do
        include Pedant::RSpec::KnifeUtil
        include Pedant::RSpec::UserUtil

        let(:org_name) { "org-#{rand(1<<32)}" }
        let(:superuser_rb) { '/etc/opscode/pivotal.rb'}
        let(:requestor) { superuser }
        let(:admin_requestor) { superuser }

        after(:each)  { knife "opc org delete #{org_name} -c #{superuser_rb} --yes" }

        context 'without associations' do
          let(:command) { "knife opc org create #{org_name} #{org_name} -c #{superuser_rb} --disable-editing" }
          it 'should succeed' do
            should have_outcome :status => 0, :stdout => /-----BEGIN (RSA )?PRIVATE KEY-----/
          end
        end

        context 'with associations' do
          let(:username) { "user-#{rand(1<<32)}" }
          let(:command) { "knife opc org create #{org_name} #{org_name} --association #{username} -c #{superuser_rb} --disable-editing" }

          before(:each) { knife "opc user create #{username} #{username} #{username} #{username}@foo.bar 'badger badger' -c #{superuser_rb} --disable-editing" }
          after(:each) { knife "opc user remove #{username} -c #{superuser_rb} --disable-editing" }

          it 'should succeed' do
            should have_outcome :status => 0, :stdout => /-----BEGIN (RSA )?PRIVATE KEY-----/
          end
        end

      end
    end
  end
end
