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

describe "Open Source /users endpoint", :users => true, :platform => :open_source do
  include Pedant::RSpec::UserUtil

  # TODO: Open-Source Chef pretends that "admin_user" is a user when it is a client
  # Now that OSC is getting real user endpoints, then this should be fixed across board.
  # Ideally, all tests should run with both clients and users as requestors.
  let(:real_admin_user) { platform.admin_user }

  let(:users_collection) { pedant_users.inject({}, &user_name_to_url) }
  let(:user_name_to_url) { ->(body, name) { body.with!(name, api_url("/users/#{name}")) } }

  context 'GET /users' do
    let(:request_method) { :GET }
    let(:request_url)    { api_url '/users' }
    let(:requestor)      { superuser }

    context 'with an operational server', :smoke do
      # For the smoke version of this test, we just want to make
      # sure that GET /users responds at all. We have no idea what
      # kind of data already exists.

      it { should look_like ok_response }
    end

    context 'with only Pedant-created users' do
      let(:expected_response) { ok_exact_response }
      let(:success_message)   { users_collection }

      should_respond_with 200, 'and the Pedant-created users'
    end
  end

  context 'GET /users/<name>' do
    let(:request_method) { :GET }
    let(:request_url)    { api_url "/users/#{real_admin_user.name}" }
    let(:requestor)      { superuser }
    let(:expected_response) { ok_full_response }

    # A successfully created or retried user will have:
    # name, public_key, openid, and the admin fields.
    #
    # (We can match on pubkey_later, as these are still tests in progress)
    #
    let(:success_message) do
      {
        "name" => real_admin_user.name,
        #public_key => 'long_key_goes_here',
        "openid" => nil,
        "admin" => true,
      }
    end

    context 'with an existing user', :smoke do
      should_respond_with 200, 'and the user'
    end
  end


  context 'POST /users' do
    include Pedant::RSpec::Validations::Create

    # Fields that can be sent to create a user:
    # Required:
    #   name, password
    # Optional:
    #   openid, admin, email (email can be set, but won't be returned back - vestige of crossover with private chef)

    after(:each) { delete_user test_user, superuser }
    let(:test_user) { "pedant_test_#{rand(100000)}" }

    let(:request_method)  { :POST }
    let(:request_url)     { api_url '/users' }
    let(:request_payload) { default_user_attributes }

    let(:expected_response) { resource_created_full_response }
    let(:created_resource) { { "uri" => resource_url } }
    let(:resource_url) { api_url "/users/#{test_user}" }
    let(:persisted_resource_response) { get(resource_url, superuser) }
    let(:default_resource_attributes) { default_user_attributes }
    let(:required_attributes) { default_user_attributes.except('admin').except('private_key') }

    let(:default_user_name) { test_user }

    context 'as an admin' do
      let(:requestor) { platform.admin_user }

      context 'without an existing user of the same name', :smoke do
        should_respond_with 201, 'and create the user' do
          # Now verify that you can retrieve it again
          persisted_resource_response.should look_like http_200_response.with(:body, request_payload.except('salt').except('password'))
          authenticate_user(default_user_name, default_user_password).should be(true)
        end
      end

      context 'with existing user of the same name' do
        before(:each) { create_user default_user_attributes.with(:name, test_user), superuser }

        let(:expected_response) { conflict_exact_response }
        let(:conflict_error_message) { ["User '#{test_user}' already exists"] }

        should_respond_with 409
      end

      context 'when validating' do
        validates_existence_of 'name'
        rejects_invalid_value_of 'name', with: 'USERNAME'
        rejects_invalid_value_of 'name', with: 'user@example.org'
        rejects_invalid_value_of 'name', with: 'user name'
        rejects_invalid_value_of 'name', with: 'user|name'

        validates_existence_of 'password'
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"

        optionally_accepts 'admin', with: true, default: false
        rejects_invalid_value_of 'admin', with: 'random string'
        rejects_invalid_value_of 'admin', with: []
        rejects_invalid_value_of 'admin', with: {}
        rejects_invalid_value_of 'admin', with: 1

        should_create_public_key
      end

      it 'cannot create a user without an authorized signing key'
      it 'sets default values for unspecified fields'

      context 'with an empty request body', :validation do
        let(:expected_response) { bad_request_response }
        let(:request_payload) { }

        should_respond_with 400
      end
    end

    context 'as a normal user', :authorization do
      let(:requestor) { platform.non_admin_user }
      let(:expected_response) { forbidden_response }

      should_respond_with 403, 'and not create the user' do
        # Now verify that the user has not been created
        persisted_resource_response.should look_like http_404_response
        authenticate_user(default_user_name, default_user_password).should be(false)
      end

      context 'with an empty request body', :validation do
        let(:expected_response) { bad_request_response }
        let(:request_payload) { }

        should_respond_with 400
      end
    end

    context 'as an non-existent user', :authentication do
      let(:requestor) { Pedant::User.new(non_existant_admin, private_key, platform: platform, preexisting: false) }
      let(:non_existant_admin) { "pedant_ghost" }
      let(:private_key) { OpenSSL::PKey::RSA.new(2048) }
      let(:public_key) { private_key.public_key.to_s }
      let(:request_payload) { default_user_attributes.with('admin', true) }

      it { should look_like http_401_response }
    end

  end # POST /users/

  context 'PUT /users/<name>' do
    include Pedant::RSpec::Validations::Update

    before(:each) { test_user_response }
    after(:each)  { delete_user test_user, superuser }

    let(:test_user) { "pedant_test_#{rand(100000)}" }
    let(:test_user_response) { create_user default_resource_attributes, superuser }
    let(:test_user_parsed_response) { parse(test_user_response) }
    let(:test_user_private_key) { test_user_parsed_response['private_key'] }
    let(:test_user_public_key) { test_user_parsed_response['public_key'] }
    let(:test_user_requestor) { Pedant::User.new(test_user, test_user_private_key, platform: platform, preexisting: false) }

    let(:request_method)  { :PUT }
    let(:request_url)     { api_url "/users/#{test_user}" }

    let(:expected_response) { ok_response }
    let(:resource_url)    { api_url "/users/#{test_user}" }
    let(:persisted_resource_response) { get(resource_url, superuser) }

    let(:default_user_name) { test_user }
    let(:default_resource_attributes) { default_user_attributes }
    let(:required_attributes) { default_user_attributes.except('admin').except('password').except('private_key') }
    let(:original_resource_attributes) do
      default_resource_attributes.
        except('password').
        except('private_key').
        with('public_key', test_user_public_key)
    end

    context 'as admin user' do
      let(:requestor)       { platform.admin_user }


      context 'with a valid update', :smoke do
        let(:request_payload) { required_attributes }
        it { should look_like ok_response }
      end

      context 'when validating' do

        validates_name
        validates_admin_flag

        # optionally_accepts password, but this requires special handling.
        # See password handling specs below
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"


        should_generate_new_keys
        should_update_public_key
        should_update_without_password
        should_update_password
      end

      context 'when modifying a normal user' do
        let(:default_resource_attributes) { default_user_attributes.with('admin', false) }

        validates_name
        validates_admin_flag
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"

        should_generate_new_keys
        should_update_public_key
        should_update_without_password
        should_update_password
      end

      context 'when modifying another admin' do
        let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

        validates_name
        validates_admin_flag
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"

        should_generate_new_keys
        should_update_public_key
        should_update_without_password
        should_update_password
      end

      context 'when setting admin flag to false on an admin user' do
        let(:request_payload) { default_user_attributes.with('admin', false) }
        let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

        should_respond_with 200, 'and update the user' do
          # Now verify that you can retrieve it again
          persisted_resource_response.should look_like http_200_response.with(:body, request_payload.except('salt').except('password'))
          authenticate_user(default_user_name, default_user_password).should be(true)
        end

        context 'when turning off admin for another admin' do
          let(:request_payload) { default_user_attributes.with('admin', false) }
          let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

          should_respond_with 200, 'and update the user' do
            # Now verify that you can retrieve it again
            persisted_resource_response.should look_like http_200_response.with(:body, request_payload.except('salt').except('password'))
            authenticate_user(default_user_name, default_user_password).should be(true)
          end
        end

        context 'when turning off admin for self' do
          let(:requestor) { test_user_requestor }
          let(:request_payload) { default_user_attributes.with('admin', false) }
          let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

          should_respond_with 200, 'and update the user' do
            # Now verify that you can retrieve it again
            persisted_resource_response.should look_like http_200_response.with(:body, request_payload.except('salt').except('password'))
            authenticate_user(default_user_name, default_user_password).should be(true)
          end
        end

        # this test is dangerous, as it tries to de-admin the last pedant admin user
        # if it succeeds in deleting the user (it shouldn't) other tests could fail
        # Note that this test will fail if other admin users exist on the system for some reason
        # ensure the only other admin in the system doesn't exist
        context 'when turning off admin for last admin', :smoke => false do
          before(:each) { delete_user(test_user) }

          let(:expected_response) { forbidden_response }
          let(:success_message) { { 'name' => platform.admin_user.name} }
          let(:request_url)       { api_url "/users/#{platform.admin_user.name}" }

          should_respond_with 403
        end # end delete with an admin user context
      end # when setting admin flag to false on an admin user


      context 'with an empty request body' do
        let(:expected_response) { bad_request_response }
        let(:request_payload) { }

        should_respond_with 400
      end

      context 'when modifying a non-existent user' do
        let(:request_url) { api_url "/users/does_not_exist" }
        let(:expected_response) { resource_not_found_response }
        let(:request_payload) { { 'name' => 'does_not_exist' } }

        should_respond_with 404

        context 'with an empty request body' do
          let(:expected_response) { bad_request_response }
          let(:request_payload) { }

          should_respond_with 400
        end
      end
    end # as admin user

    context 'as admin client' do
      let(:requestor)       { platform.admin_client }

      context 'when validating' do

        validates_name
        validates_admin_flag

        # optionally_accepts password, but this requires special handling.
        # See password handling specs below
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"

        should_generate_new_keys
        should_update_public_key
        should_update_without_password
        should_update_password
      end

      context 'when modifying a normal user' do
        let(:default_resource_attributes) { default_user_attributes.with('admin', false) }

        validates_name
        validates_admin_flag
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"

        should_generate_new_keys
        should_update_public_key
        should_update_without_password
        should_update_password
      end

      context 'when modifying another admin' do
        let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

        validates_name
        validates_admin_flag
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"

        should_generate_new_keys
        should_update_public_key
        should_update_without_password
        should_update_password
      end

      context 'when setting admin flag to false on an admin user' do
        let(:request_payload) { default_user_attributes.with('admin', false) }
        let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

        should_respond_with 200, 'and update the user' do
          # Now verify that you can retrieve it again
          persisted_resource_response.should look_like http_200_response.with(:body, request_payload.except('salt').except('password'))
          authenticate_user(default_user_name, default_user_password).should be(true)
        end

        context 'when turning off admin for another admin' do
          let(:request_payload) { default_user_attributes.with('admin', false) }
          let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

          should_respond_with 200, 'and update the user' do
            # Now verify that you can retrieve it again
            persisted_resource_response.should look_like http_200_response.with(:body, request_payload.except('salt').except('password'))
            authenticate_user(default_user_name, default_user_password).should be(true)
          end
        end

        context 'when turning off admin for self' do
          let(:requestor) { test_user_requestor }
          let(:request_payload) { default_user_attributes.with('admin', false) }
          let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

          should_respond_with 200, 'and update the user' do
            # Now verify that you can retrieve it again
            persisted_resource_response.should look_like http_200_response.with(:body, request_payload.except('salt').except('password'))
            authenticate_user(default_user_name, default_user_password).should be(true)
          end
        end

        # this test is dangerous, as it tries to de-admin the last pedant admin user
        # if it succeeds in deleting the user (it shouldn't) other tests could fail
        # Note that this test will fail if other admin users exist on the system for some reason
        # ensure the only other admin in the system doesn't exist
        # BUG: Marked as skip: admin clients can turn off the last user admin
        context 'when turning off admin for last admin', skip: "CHEF-3658: admin clients should not be able to de-admin admin user", smoke: false do
          before(:each) { delete_user(test_user) }

          let(:expected_response) { forbidden_response }
          let(:success_message) { { 'name' => platform.admin_user.name} }
          let(:request_url)       { api_url "/users/#{platform.admin_user.name}" }

          should_respond_with 403
        end # end delete with an admin user context
      end # when setting admin flag to false on an admin user


      context 'with an empty request body' do
        let(:expected_response) { bad_request_response }
        let(:request_payload) { }

        should_respond_with 400
      end

      context 'when modifying a non-existent user' do
        let(:request_url) { api_url "/users/does_not_exist" }
        let(:expected_response) { resource_not_found_response }
        let(:request_payload) { { 'name' => 'does_not_exist' } }

        should_respond_with 404

        context 'with an empty request body' do
          let(:expected_response) { bad_request_response }
          let(:request_payload) { }

          should_respond_with 400
        end
      end
    end # as admin client

    context 'as normal user' do
      let(:requestor) { test_user_requestor }
      let(:default_resource_attributes) { default_user_attributes.with('admin', false) }
      let(:original_resource_attributes) do
        default_resource_attributes.
          except('password').
          except('private_key').
          with('public_key', test_user_public_key)
      end

      context 'when modifying self' do


        context 'with a valid update' do
          let(:request_payload) { required_attributes }
          it { should look_like ok_response }
        end

        validates_name

        # optionally_accepts password, but this requires special handling.
        # See password handling specs below
        validates_length_of 'password', min: 6, error_message: "Password must have at least 6 characters"

        # It is ok for a normal user to pass 'admin' => false
        optionally_accepts 'admin', with: false, default: false

        # It is not ok for a normal user to escale privs
        forbids_update_to 'admin', with: true, default: false

        # Some extra priv escalation tests are appended below.

        should_generate_new_keys
        should_update_public_key
        should_update_without_password
        should_update_password
      end # when modifying self

      context 'when modifying another normal user' do
        let(:requestor) { platform.non_admin_user }
        let(:expected_response) { forbidden_response }

        # Should not be able to modify another user
        forbids_update_to 'name', with: 'username'
        forbids_update_to 'private_key', with: true
        forbids_update_to 'public_key', with: random_public_key
        forbids_update_to 'password', with: random_text

        # Tests priv escalation
        forbids_update_to 'admin', with: true
      end

      context 'when modifying another admin user' do
        let(:requestor) { platform.non_admin_user }
        let(:expected_response) { forbidden_response }
        let(:request_payload) { default_user_attributes.with('admin', false) }
        let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

        # Should not be able to modify another user
        forbids_update_to 'name', with: 'username'
        forbids_update_to 'private_key', with: true
        forbids_update_to 'public_key', with: random_public_key
        forbids_update_to 'password', with: random_text

        # Tests priv escalation
        forbids_update_to 'admin', with: true

        # this test is dangerous, as it tries to de-admin the last pedant admin user
        # if it succeeds in deleting the user (it shouldn't) other tests could fail
        # Note that this test will fail if other admin users exist on the system for some reason
        # ensure the only other admin in the system doesn't exist
        context 'when turning off admin for last admin', :smoke => false do
          before(:each) { delete_user test_user }
          let(:expected_response) { forbidden_response }
          let(:success_message) { { 'name' => platform.admin_user.name} }
          let(:request_url)       { api_url "/users/#{platform.admin_user.name}" }

          should_respond_with 403
        end # end delete with an admin user context
      end # when setting admin flag to false on an admin user


      context 'with an empty request body' do
        let(:expected_response) { bad_request_response }
        let(:request_payload) { }

        should_respond_with 400
      end

      context 'when modifying a non-existent user' do
        let(:request_url) { api_url "/users/does_not_exist" }
        let(:expected_response) { resource_not_found_response }
        let(:request_payload) { { 'name' => 'does_not_exist' } }

        should_respond_with 404

        context 'with an empty request body' do
          let(:expected_response) { bad_request_response }
          let(:request_payload) { }

          should_respond_with 400
        end
      end
    end # as normal user

    context 'as normal user' do
      let(:requestor) { platform.non_admin_client }
      let(:default_resource_attributes) { default_user_attributes.with('admin', false) }
      let(:original_resource_attributes) do
        default_resource_attributes.
          except('password').
          except('private_key').
          with('public_key', test_user_public_key)
      end

      context 'when modifying another normal user' do
        let(:requestor) { platform.non_admin_user }
        let(:expected_response) { forbidden_response }

        # Should not be able to modify another user
        forbids_update_to 'name', with: 'username'
        forbids_update_to 'private_key', with: true
        forbids_update_to 'public_key', with: random_public_key
        forbids_update_to 'password', with: random_text

        # Tests priv escalation
        forbids_update_to 'admin', with: true
      end

      context 'when modifying another admin user' do
        let(:requestor) { platform.non_admin_user }
        let(:expected_response) { forbidden_response }
        let(:request_payload) { default_user_attributes.with('admin', false) }
        let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

        # Should not be able to modify another user
        forbids_update_to 'name', with: 'username'
        forbids_update_to 'private_key', with: true
        forbids_update_to 'public_key', with: random_public_key
        forbids_update_to 'password', with: random_text

        # Tests priv escalation
        forbids_update_to 'admin', with: true

        # this test is dangerous, as it tries to de-admin the last pedant admin user
        # if it succeeds in deleting the user (it shouldn't) other tests could fail
        # Note that this test will fail if other admin users exist on the system for some reason
        # ensure the only other admin in the system doesn't exist
        context 'when turning off admin for last admin', :smoke => false do
          before(:each) { delete_user test_user }
          let(:expected_response) { forbidden_response }
          let(:success_message) { { 'name' => platform.admin_user.name} }
          let(:request_url)       { api_url "/users/#{platform.admin_user.name}" }

          should_respond_with 403
        end # end delete with an admin user context
      end # when setting admin flag to false on an admin user


      context 'with an empty request body' do
        let(:expected_response) { bad_request_response }
        let(:request_payload) { }

        should_respond_with 400
      end

      context 'when modifying a non-existent user' do
        let(:request_url) { api_url "/users/does_not_exist" }
        let(:expected_response) { resource_not_found_response }
        let(:request_payload) { { 'name' => 'does_not_exist' } }

        should_respond_with 404

        context 'with an empty request body' do
          let(:expected_response) { bad_request_response }
          let(:request_payload) { }

          should_respond_with 400
        end
      end
    end # as normal user

    context 'as an non-existent user', :authentication do
      let(:requestor) { Pedant::User.new(non_existant_admin, private_key, platform: platform, preexisting: false) }
      let(:non_existant_admin) { "pedant_ghost" }
      let(:private_key) { OpenSSL::PKey::RSA.new(2048) }
      let(:public_key) { private_key.public_key.to_s }
      let(:request_payload) { default_user_attributes.with('admin', false) }
      let(:default_resource_attributes) { default_user_attributes.with('admin', true) }

      it { should look_like http_401_response }
    end

  end # PUT /users/<name>

  context 'DELETE /users/<name>' do
    let(:request_method) { :DELETE }
    let(:default_user_name) { "user" }

    context 'user to be deleted is admin' do
      let(:request_url)    { api_url "/users/#{test_user}" }
      let(:test_user_name) { "pedant_test_user_#{rand(10000)}" }
      let(:test_user) { test_user_name.tap(&create_user!) }
      let(:success_message) { { 'name' => test_user } }
      let(:requestor)      { superuser }
      let(:create_user!) { ->(name) { create_user default_user_attributes.with('name', name), superuser } }
      after(:each) { delete_user(test_user) }

      context 'as an admin client'  do
        let(:expected_response) { ok_full_response }
        # clients right now dont have any limiting perms
        context 'with an existing admin user', :smoke do
          should_respond_with 200, 'and delete the user'
        end

        context 'with non-existent user' do
          before(:each) { delete_user(test_user_name) }
          let(:request_url)       { api_url "/users/#{test_user_name}" }
          let(:expected_response) { resource_not_found_response }

          should_respond_with 404
        end # end with non-existend user
      end # end delete with admin client context

      # This tests that a non-admin user cannot delete an admin user
      context 'as a non-admin user' do
        let(:requestor) { platform.non_admin_user }
        let(:expected_response) { forbidden_response }

        should_respond_with 403, 'and not delete user'

        context 'with non-existent user' do
          before(:each) { delete_user(test_user_name) }
          let(:request_url)       { api_url "/users/#{test_user_name}" }
          let(:expected_response) { resource_not_found_response }

          should_respond_with 404
        end # end non-existend user context
     end # end non-admin user context

      context 'as an admin user' do
        let(:requestor) { platform.admin_user }
        let(:expected_response) { ok_full_response }

        context 'with an existing admin user', :smoke do
          should_respond_with 200
        end

        # this test is dangerous, as it tries to delete the main pedant admin user
        # if it succeeds in deleting the user (it shouldn't) other tests could fail
        # Note that this test will fail if other admin users exist on the system for some reason
        context 'try deleting last admin', :smoke => false do
          # ensure the only other admin in the system doesn't exist
          before(:each) { delete_user(test_user_name) }

          let(:expected_response) { forbidden_response }
          let(:success_message) { { 'name' => platform.admin_user.name} }
          let(:request_url)       { api_url "/users/#{platform.admin_user.name}" }

          should_respond_with 403
        end

      end # end delete with an admin user context

    end # end user to be deleted is admin context

    context 'user to be deleted is non-admin' do
      let(:request_url) { api_url "/users/#{another_non_admin_user}"}
      after(:each) { delete_user another_non_admin_user }
      let(:requestor) { platform.non_admin_user }

      let(:another_non_admin_user) do
        "another_pedant_non_admin_user_#{rand(1000)}".tap do |name|
          create_user default_user_attributes.with('name', name).with('admin', false), superuser
        end
      end

      context 'non-admin can delete themselves', :smoke do
        let(:non_admin_user) { parse(create_user default_user_attributes.with('name', non_admin_user_name).with('admin', false), superuser) }
        let(:non_admin_user_name){ "pedant_non_admin_user_#{rand(10000)}" }
        let(:success_message) { { 'name' => non_admin_user_name} }
        let(:request_url)       { api_url "/users/#{non_admin_user_name}" }
        # Set a non-admin user we can delete as the requestor and have them delete themselves
        let(:requestor) { Pedant::Requestor.new( non_admin_user_name, non_admin_user["private_key"] ) }
        let(:expected_response) { ok_full_response }

        should_respond_with 200
      end # end non-admin can delete themselves

      skip 'cannot be deleted by another non-admin user' do
        let(:expected_response) { forbidden_response }

        should_respond_with 403
      end

      context 'with non-existent user' do
        let(:expected_response) { forbidden_response}

        should_respond_with 403
      end # end non-existend user context

      #end # end non-admin user context

    end # end user to be deleted is non-admin context

  end # end DELETE context

end
