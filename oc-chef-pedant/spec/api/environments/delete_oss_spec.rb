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

describe "Environments API Endpoint", :environments do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::AuthHeadersUtil

  let(:new_environment_name) { 'pedant_testing_environment' }
  let(:non_existent_environment_name) { 'pedant_dummy_environment' }

  let(:requestor) { admin_user }

  context 'with no additional environments' do

    before(:each) { delete_environment(admin_user, new_environment_name) }
    after(:each) { delete_environment(admin_user, new_environment_name) }

    context 'DELETE /environments' do
      let(:request_method) { :DELETE }
      let(:request_url)    { api_url '/environments' }

      let(:expected_response) { method_not_allowed_response }
      should_respond_with 405
    end # DELETE /environments

    describe "DELETE /environments/<name>" do
      let(:request_method) { :DELETE }
      let(:request_url)    { api_url "/environments/#{environment_name}" }

      context 'with "_default" environment' do
        let(:environment_name) { '_default' }
        let(:expected_response) { method_not_allowed_exact_response }
        let(:error_message) { ["The '_default' environment cannot be modified."] }

        should_respond_with 405
      end

      context 'with non-existent environment' do
        let(:environment_name) { non_existent_environment_name }
        let(:expected_response) { resource_not_found_exact_response }
        let(:not_found_error_message) { ["Cannot load environment #{non_existent_environment_name}"] }

        should_respond_with 404
      end
    end # DELETE /environments/<name>
  end # without additional non-default environments

  context 'with non-default environments in the organization' do

    before(:each) { add_environment(admin_user, full_environment(new_environment_name)) }
    after(:each)  { delete_environment(admin_user, new_environment_name) }

    describe 'DELETE /environments/<name>' do
      let(:request_method) { :DELETE }
      let(:request_url)    { api_url "/environments/#{environment_name}" }

      let(:environment_name) { new_environment_name }

      context 'when authenticating', :pedantic do
        # Unconverted Auth Headers util DSL
        let(:method) { request_method}
        let(:url)    { request_url }
        let(:body)   { nil }
        let(:response_should_be_successful) do
          response.
            should look_like({ :status => 200 })
        end
        let(:success_user) { admin_user }
        let(:failure_user) { outside_user }

        include_context 'handles authentication headers correctly'
      end

      context 'with an existing environment', :smoke do
        let(:environment_name) { new_environment_name }
        it { should look_like ok_response }
      end

      context 'without an existing environment' do
        let(:environment_name) { 'doesnotexistatall' }
        let(:expected_response) { resource_not_found_response }
        should_respond_with 404
      end

      context 'when attempting to delete "_default" environment' do
        let(:environment_name) { '_default' }
        let(:expected_response) { method_not_allowed_response }
        should_respond_with 405
      end
    end

  end

end
