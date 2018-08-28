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

require 'pedant/rspec/auth_headers_util'
require 'pedant/rspec/environment_util'
require 'pedant/rspec/role_util'

describe "Environments API Endpoint", :environments, :roles do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::AuthHeadersUtil

  # include roles for testing w/ environments roles method
  include Pedant::RSpec::RoleUtil

  # TODO: pull this up somewhere else, eventually
  let(:organizations_url){ "#{server}/organizations" }

  # Deal with subtly different error messages/codes in one place

  let(:outside_user_not_associated_msg) {
    ["'#{outside_user.name}' not associated with organization '#{org}'"] }

  let(:new_environment_name) { 'pedant_testing_environment' }
  let(:non_existent_environment_name) { 'pedant_dummy_environment' }

  # TODO: Should turn this into a macro or a shared_context
  describe "GET /environments/<name>/roles" do
    let(:request_method) { :GET }
    let(:requestor)      { admin_user }
    let(:request_url) { api_url("/environments/#{environment_name}/roles/#{role_name}") }

    let(:environment_name) { new_environment_name }
    let(:role_name) { new_role_name }

    let(:new_role_name) { "webserver" }
    let(:new_environment_name) { 'pedant_testing_environment' }
    let(:non_existent_environment_name) { 'pedant_dummy_environment' }

    let(:fetch_role_exact_response) { http_200_response.with(:body_exact => { 'run_list' => run_list }) }
    let(:role_not_found_in_environment_exact_response) { http_200_response.with(:body_exact => { 'run_list' => nil }) }

    let(:role_with_no_env) { new_role(new_role_name, {}, { run_list: run_list } ) }
    let(:role_with_env) { new_role(new_role_name, {}, { env_run_lists: env_run_lists } ) }

    let(:run_list) { [ "recipe[apache2]" ] }
    let(:env_run_lists) { { new_environment_name => run_list } }

    let(:create_role_with_env!) { add_role(admin_user, role_with_env) }
    let(:create_role_without_env!) { add_role(admin_user, role_with_no_env) }

    after :each do
      delete_environment(admin_user, new_environment_name)
      delete_role(admin_user, new_role_name)
    end

    context 'within the default environment' do
      before(:each) { create_role_without_env! }
      let(:environment_name) { '_default' }
      let(:expected_response) { fetch_role_exact_response }

      let(:run_list) { ["recipe[nginx]"] }

      context 'with an existing role' do
        should_respond_with 200
      end

      context 'without an existing role' do
        let(:request_url) { api_url("/environments/_default/roles/#{role_name}") }
        let(:expected_response) { resource_not_found_exact_response }
        let(:not_found_error_message) { ["Cannot load role #{role_name}"] }
        let(:role_name) { 'not_a_role' }

        should_respond_with 404
      end
    end


    context 'within a non-default environment' do
      before(:each) { create_environment!; create_role_with_env! }
      let(:expected_response) { fetch_role_exact_response }

      let(:environment_name) { new_environment_name }
      let(:role_name) { new_role_name }

      let(:create_environment!) { add_environment(admin_user, new_environment(new_environment_name)) }

      context 'with an existing role', :smoke do
        should_respond_with 200, 'and the role'
      end

      context 'with non-existent environment' do
        before(:each) { create_role_with_env! }
        let(:expected_response) { environment_not_found_response }
        let(:environment_not_found_message){environment_not_found_message_alternate}
        let(:environment_name) { non_existent_environment_name }

        should_respond_with 404
      end

      context 'without authorization', :authorization do
        let(:expected_response) { unauthorized_access_credential_response }
        let(:requestor) { outside_user }

        should_respond_with 401
      end # without authorization

      context 'with existing role in different existing environment' do
        before(:each) { create_role_without_env! }

        let(:expected_response) { role_not_found_in_environment_exact_response }
        let(:run_list) { ["recipe[nginx]"] }

        should_respond_with 404
      end # with existing role in different existing environment
    end # with non-default environment

  end # GET /environments/:environment/roles/:role
end
