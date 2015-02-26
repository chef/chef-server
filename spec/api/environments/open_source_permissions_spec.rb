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

require 'pedant/opensource/permission_checks'
require 'pedant/rspec/cookbook_util'
require 'pedant/rspec/environment_util'
require 'pedant/rspec/role_util'

describe 'Environments Endpoint Open Source Permission Checks', :platform => :open_source do
  include Pedant::RSpec::CookbookUtil
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::RoleUtil
  include Pedant::OpenSource::PermissionChecks
  include_context 'environment_body_util'

  # Cookbook tests are parameterized to support common testing of both
  # /cookbooks and /cookbook_artifacts, so we need to specify that we want to
  # talk to /cookbooks
  let(:cookbook_url_base) { "cookbooks" }

  # Until we rename the requestors
  let(:admin_requestor){admin_user}

  context '/environments' do
    let(:request_url){api_url('/environments')}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    context 'POST' do
      let(:request_method){:POST}
      let(:environment_name){unique_name('testing_environment')}
      let(:request_payload){new_environment(environment_name)}

      # TODO: Extract this 'after each' behavior into a context?
      after :each do
        delete_environment(admin_requestor, environment_name)
      end

      include_context 'permission checks' do
        let(:admin_response){created_response}
        let(:non_admin_response){forbidden_response}
      end
    end

    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end # /environments

  context '/environments/<environment>' do
    let(:request_url){api_url("/environments/#{environment_name}")}
    include_context 'with temporary testing environment'

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
      # TODO: empty_payload defined in environment_body_util context; consider refactoring
      # (Minimal payload that will work)
      let(:request_payload){empty_payload.tap{|p| p['name'] = environment_name}}
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

  end # /environments/<environment>

  context '/environments/<environment>/cookbooks' do
    let(:request_url){api_url("/environments/#{environment_name}/cookbooks")}
    include_context 'with temporary testing environment'

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
  end  # /environments/<environment>/cookbooks

  context  '/environments/<environment>/cookbooks/<cookbook>' do
    let(:request_url){api_url("/environments/#{environment_name}/cookbooks/#{cookbook_name}")}
    include_context 'with temporary testing environment'
    include_context 'with temporary testing cookbook'

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
  end # /environments/<environment>/cookbooks/<cookbook>

  context '/environments/<environment>/recipes' do
    let(:request_url){api_url("/environments/#{environment_name}/recipes")}
    include_context 'with temporary testing environment'

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
  end # /environments/<environment>/recipes

  context '/environments/<environment>/nodes' do
    let(:request_url){api_url("/environments/#{environment_name}/nodes")}
    include_context 'with temporary testing environment'

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
  end # /environments/<environment>/nodes

  context '/environments/<environment>/roles/<role>' do
    let(:request_url){api_url("/environments/#{environment_name}/roles/#{role_name}")}
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
  end  # /environments/<environment>/roles/<role>

  # Depsolver endpoint
  context '/environments/<environment>/cookbook_versions' do
    let(:request_url){api_url("/environments/#{environment_name}/cookbook_versions")}
    include_context 'with temporary testing environment'
    # An empty run list is the simplest thing that could work
    let(:request_payload){{'run_list' => []}}

    should_not_allow_method :GET

    context 'POST' do
      let(:request_method){:POST}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end # /environments/<environment>/cookbook_versions

end
