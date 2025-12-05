# -*- coding: utf-8 -*-
# Copyright Chef Software, Inc. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License. You may obtain
# a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.

require 'pedant/rspec/common'

describe 'Users API', :users do
  # Generate a random 7-8 digit number (standard pedant pattern)
  def rand_id
    rand(10**7...10**8).to_s
  end

  describe 'disabled user functionality' do
    let(:request_method) { :GET }
    let(:request_url) { api_url("/users/#{username}") }
    let(:requestor) { superuser }
    let(:username) { "pedant-disabled-test-#{rand_id}" }
    let(:user_payload) do
      {
        'username' => username,
        'email' => "#{username}@example.com",
        'first_name' => username,
        'last_name' => username,
        'display_name' => username,
        'password' => 'Test1234!@#$'
      }
    end

    # Helper to create update payload with disabled field
    def update_payload_with_disabled(username_param, disabled_value)
      {
        'username' => username_param,
        'email' => "#{username_param}@example.com",
        'first_name' => username_param,
        'last_name' => username_param,
        'display_name' => username_param,
        'disabled' => disabled_value
      }
    end

    context 'when creating a new user' do
      after(:each) do
        delete_user(username, superuser) rescue nil
      end

      it 'ignores disabled field in POST request (always creates with disabled=false)' do
        payload_with_disabled = user_payload.merge('disabled' => true)
        post("#{platform.server}/users", superuser, payload: payload_with_disabled) do |response|
          response.should look_like({
            status: 201
          })
        end

        # Verify user was created with disabled=false
        get("#{platform.server}/users/#{username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'disabled' => false
            }
          })
        end
      end

      it 'ignores invalid disabled value in POST request (always creates with disabled=false)' do
        payload_with_invalid_disabled = user_payload.merge('disabled' => 'not-a-boolean')
        post("#{platform.server}/users", superuser, payload: payload_with_invalid_disabled) do |response|
          response.should look_like({
            status: 201
          })
        end

        # Verify user was created with disabled=false despite invalid value
        get("#{platform.server}/users/#{username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'disabled' => false
            }
          })
        end
      end

      it 'creates user with disabled=false by default' do
        post("#{platform.server}/users", superuser, payload: user_payload) do |response|
          response.should look_like({
            status: 201
          })
        end

        get("#{platform.server}/users/#{username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'username' => username,
              'disabled' => false
            }
          })
        end
      end
    end

    context 'when updating a user' do
      before(:each) do
        post("#{platform.server}/users", superuser, payload: user_payload)
      end

      after(:each) do
        delete_user(username, superuser) rescue nil
      end

      it 'allows admin to set disabled=true via PUT' do
        put("#{platform.server}/users/#{username}", superuser, payload: update_payload_with_disabled(username, true)) do |response|
          response.should look_like({
            status: 200
          })
        end

        # Verify disabled was updated
        get("#{platform.server}/users/#{username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'disabled' => true
            }
          })
        end
      end

      it 'allows admin to set disabled=false via PUT' do
        # First set to true
        put("#{platform.server}/users/#{username}", superuser, payload: update_payload_with_disabled(username, true))

        # Then set back to false
        put("#{platform.server}/users/#{username}", superuser, payload: update_payload_with_disabled(username, false)) do |response|
          response.should look_like({
            status: 200
          })
        end

        get("#{platform.server}/users/#{username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'disabled' => false
            }
          })
        end
      end

      it 'rejects invalid disabled value with 400' do
        invalid_payload = {
          'username' => username,
          'email' => "#{username}@example.com",
          'first_name' => username,
          'last_name' => username,
          'display_name' => username,
          'disabled' => 'not-a-boolean'
        }
        put("#{platform.server}/users/#{username}", superuser, payload: invalid_payload) do |response|
          response.should look_like({
            status: 400
          })
        end
      end
    end

    context 'when user is disabled' do
      let(:disabled_username) { "pedant-disabled-#{rand_id}" }
      let(:disabled_user_payload) do
        {
          'username' => disabled_username,
          'email' => "#{disabled_username}@example.com",
          'first_name' => disabled_username,
          'last_name' => disabled_username,
          'display_name' => disabled_username,
          'password' => 'Test1234!@#$'
        }
      end
      let(:disabled_user_private_key) { @disabled_user_private_key }

      before(:each) do
        # Create user and capture the private key from response
        post("#{platform.server}/users", superuser, payload: disabled_user_payload) do |response|
          response.should look_like({
            status: 201
          })
          @disabled_user_private_key = JSON.parse(response.body)['private_key']
        end
        # Disable the user
        put("#{platform.server}/users/#{disabled_username}", superuser, payload: update_payload_with_disabled(disabled_username, true))
      end

      after(:each) do
        delete_user(disabled_username, superuser) rescue nil
        @disabled_user_private_key = nil
      end

      it 'returns 403 with user_disabled error when disabled user tries to access their own user record' do
        # Create a requestor for the disabled user using their captured private key
        disabled_requestor = Pedant::Requestor.new(
          disabled_username,
          disabled_user_private_key,
          platform: platform
        )

        get("#{platform.server}/users/#{disabled_username}", disabled_requestor) do |response|
          response.should look_like({
            status: 403,
            body: {
              'error' => 'user_disabled'
            }
          })
        end
      end

      it 'returns 403 when disabled user tries to list users' do
        disabled_requestor = Pedant::Requestor.new(
          disabled_username,
          disabled_user_private_key,
          platform: platform
        )

        get("#{platform.server}/users", disabled_requestor) do |response|
          response.should look_like({
            status: 403,
            body: {
              'error' => 'user_disabled'
            }
          })
        end
      end

      it 'allows admin to access disabled user record' do
        get("#{platform.server}/users/#{disabled_username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'username' => disabled_username,
              'disabled' => true
            }
          })
        end
      end

      it 'allows admin to re-enable the user' do
        put("#{platform.server}/users/#{disabled_username}", superuser, payload: update_payload_with_disabled(disabled_username, false)) do |response|
          response.should look_like({
            status: 200
          })
        end

        # Verify user can now access their record
        disabled_requestor = Pedant::Requestor.new(
          disabled_username,
          disabled_user_private_key,
          platform: platform
        )

        get("#{platform.server}/users/#{disabled_username}", disabled_requestor) do |response|
          response.should look_like({
            status: 200,
            body: {
              'disabled' => false
            }
          })
        end
      end
    end

    context 'GET /users/:name returns disabled field' do
      before(:each) do
        post("#{platform.server}/users", superuser, payload: user_payload)
      end

      after(:each) do
        delete_user(username, superuser) rescue nil
      end

      it 'includes disabled=false in response for enabled user' do
        get("#{platform.server}/users/#{username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'username' => username,
              'disabled' => false
            }
          })
        end
      end

      it 'includes disabled=true in response for disabled user' do
        put("#{platform.server}/users/#{username}", superuser, payload: update_payload_with_disabled(username, true))

        get("#{platform.server}/users/#{username}", superuser) do |response|
          response.should look_like({
            status: 200,
            body: {
              'username' => username,
              'disabled' => true
            }
          })
        end
      end
    end
  end
end
