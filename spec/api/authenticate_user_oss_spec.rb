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

require 'pedant/rspec/user_util'
require 'pedant/rspec/validations'
require 'securerandom'

describe "Open source /authenticate_users endpoint", :users => true, :platform => :open_source do
  include Pedant::RSpec::UserUtil

  # TODO: Open-Source Chef pretends that "admin_user" is a user when it is a client
  # Now that OSC is getting real user endpoints, then this should be fixed across board.
  # Ideally, all tests should run with both clients and users as requestors.
  let(:real_admin_user) { platform.admin_user }

  let(:users_collection) { ->(names) { names.inject({}, &user_name_to_url) } }
  let(:user_name_to_url) { ->(body, name) { body.with!(name, api_url("/users/#{name}")) } }
  let(:pedant_users) { platform.users.map(&:name).sort }

  context 'POST /authenticate_user' do
    let(:request_method) { :POST }
    let(:request_url)    { api_url '/authenticate_user' }
    let(:requestor)      { superuser }

    let(:default_user_name) { 'pedant_test_alice' }
    let(:default_user_password) { SecureRandom.hex(16) }

    context 'with existing user' do
      before(:each) { create_user(default_user_attributes.with('admin', false), requestor) }
      after(:each) { delete_user(default_user_name) }

      context 'and correct password', :smoke do
        let(:expected_response) { ok_full_response }
        let(:success_message) do
          {
            'name'     => default_user_name,
            'verified' => true
          }
        end

        let(:request_payload) do
          {
            'name'     => default_user_name,
            'password' => default_user_password
          }
        end

        should_respond_with 200, 'verified => true'
      end # context

      context 'and wrong password', :smoke do
        let(:expected_response) { ok_full_response }
        let(:success_message) do
          {
            'name'     => default_user_name,
            'verified' => false
          }
        end

        let(:request_payload) do
          {
            'name' => default_user_name,
            'password' => 'not the password'
          }
        end

        should_respond_with 200, 'verified => false'
      end # context
    end

    context 'with a non-existing user' do
      let(:no_such_user) { "no-such-user-#{unique_suffix}" }
      let(:expected_response) { ok_full_response }
      let(:success_message) do
        {
          'name'     => no_such_user,
          'verified' => false
        }
      end

      let(:request_payload) do
        {
          'name' => no_such_user,
          'password' => 'not the password'
        }
      end

      should_respond_with 200, 'verified => false'
    end # context

    context 'with a bad request', :validation do
      let(:expected_response) { http_400_exact_response }
      context 'missing password' do
        let(:error_message) { ["Field 'password' missing"] }

        let(:request_payload) do
          {
            'name' => 'alice'
          }
        end
        should_respond_with 400
      end

      context 'missing name' do
        let(:error_message) { ["Field 'name' missing"] }
        let(:request_payload) do
          {
            'password' => 'topsecret'
          }
        end
        should_respond_with 400
      end

      context 'bad password' do
        let(:error_message) { ["Field 'password' invalid"] }
        let(:request_payload) do
          {
            'name' => 'alice',
            'password' => [1, 2, 3]
          }
        end
        should_respond_with 400
      end

      context 'bad name' do
        let(:error_message) { ["Field 'name' invalid"] }
        let(:request_payload) do
          {
            'name' => {'alice' => true},
            'password' => 'abcdef'
          }
        end
        should_respond_with 400
      end

    end # context

  end #end POST context
end
