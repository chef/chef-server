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

require 'pedant/rspec/environment_util'
require 'pedant/rspec/role_util'
require 'pedant/opensource/permission_checks'

describe 'Role Endpoint Open Source Permission Checks', :platform => :open_source, :roles => true do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::RoleUtil
  include Pedant::OpenSource::PermissionChecks

  # Until we rename the requestors
  let(:admin_requestor){admin_user}

  context '/roles' do
    let(:request_url){api_url('/roles')}
    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end
    context 'POST' do
      let(:request_method){:POST}
      let(:role_name){"testing_role"}
      let(:request_payload){ new_role(role_name)}

      after :each do
        delete_role(admin_requestor, role_name)
      end

      include_context 'permission checks' do
        let(:admin_response){created_response}
        let(:non_admin_response){forbidden_response}
      end
    end

    should_not_allow_method :PUT
    should_not_allow_method :DELETE

  end # /roles

  context '/roles/<role>' do
    include_context 'with temporary testing role'
    let(:request_url){api_url("/roles/#{role_name}")}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST

    context 'PUT' do
      let(:request_method){:PUT}
      let(:minimal_role_update) do
        {
          "json_class" => "Chef::Role",
          "description" => "My Pedantic Role"
        }
      end

      let(:request_payload){minimal_role_update}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){forbidden_response}
      end
    end
    context 'DELETE' do
      let(:request_method){:DELETE}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){forbidden_response}
      end
    end
  end # /roles/<role>

  context '/roles/<role>/environments', :environments do
    let(:request_url){api_url("/roles/#{role_name}/environments")}
    include_context 'with temporary testing environment'
    include_context 'with temporary testing role'

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end  # /roles/<role>/environments

  context '/roles/<role>/environments/<environment>', :environments do
    let(:request_url){api_url("/roles/#{role_name}/environments/#{environment_name}")}
    include_context 'with temporary testing environment'
    include_context 'with temporary testing role'

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end  # /roles/<role>/environments/<environment>
end
