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

require 'pedant/rspec/open_source_client_util'
require 'pedant/rspec/validations'

# Since we've already got an open-source only test spec for the
# clients endpoint, and since many of the operations that can be done
# are sensitive to the permissions of the requestor, we'll just
# integrate the permissions checking in with the rest of the tests.
require 'pedant/opensource/permission_checks'

describe "Open Source Client API endpoint", :platform => :open_source, :clients => true do
  include Pedant::RSpec::OpenSourceClientUtil
  include Pedant::OpenSource::PermissionChecks

  # Just until we rename the requestors
  let(:admin_requestor)  { admin_user }
  let(:normal_requestor) { normal_user }
  # TODO: Pull this out

  let(:validator_client){ Pedant::Client.new(platform.validator_client_name,
                                             "/etc/chef-server/chef-validator.pem")}
  let(:requestor)   { admin_requestor }
  let(:client_name) { pedant_admin_client_name }

  shared_context 'non-admin clients cannot perform operation' do
    let(:not_allowed_response){fail 'Please define not_allowed_response'}

    context 'as a non-admin client', :authorization do
      let(:requestor) { normal_requestor }
      it { should look_like not_allowed_response }
    end

    context 'as a validator client', :authorization do
      let(:requestor) { validator_client }
      it { should look_like not_allowed_response }
    end
  end

  def self.non_admin_clients_cannot_update
    include_context 'non-admin clients cannot perform operation' do
      let(:not_allowed_response){update_client_as_non_admin_response}
    end
  end

  def self.non_admin_clients_cannot_delete
    include_context 'non-admin clients cannot perform operation' do
      let(:not_allowed_response){delete_client_as_non_admin_response}
    end
  end


  # Other "container" endpoints are tested with and without items
  # already in the system, but there are always clients present in the
  # system, so we don't make that distinction.

  context 'GET /clients' do
    let(:request_method) { :GET }
    let(:request_url)    { clients_url }

    let(:clients_collection) { pedant_clients.inject({}, &client_name_to_url) }
    let(:client_name_to_url) { ->(body, name) { body.with!(name, api_url("/clients/#{name}")) } }

    # TODO: Do this when we fix the look_like matcher
    # include_context 'permission checks' do
    #   let(:admin_response){fetch_prepopulated_clients_success_response}
    #   let(:non_admin_response){forbidden_response}
    # end

    context 'as an admin client' do
      let(:requestor){admin_requestor}
      let(:expected_response) { ok_response }

      context 'with an operational server', :smoke do
        it { should look_like ok_response }
      end

      context 'with only Pedant-created clients' do
        let(:expected_response) { ok_exact_response }
        let(:success_message)   { clients_collection }

        should_respond_with 200, 'and the Pedant-created clients'
      end
    end
  end

  context 'POST /clients' do
    include Pedant::RSpec::Validations::Create

    let(:request_method)  { :POST }
    let(:request_url)     { clients_url }
    let(:request_payload) { default_client_attributes }

    let(:client_name) { unique_name("testclient") }
    let(:client_is_admin) { false }
    let(:default_client_attributes) do
      {
        "name" => client_name,
        "admin" => client_is_admin
      }
    end

    # useful for checking the result of a create operation
    # TODO: Refactor to resource_url
    let(:client_url) { api_url("/clients/#{client_name}") }

    let(:expected_response) { resource_created_full_response }
    let(:created_resource) { { "uri" => resource_url } }
    let(:resource_url) { client_url }
    let(:persisted_resource_response) { get(resource_url, superuser) }
    let(:default_resource_attributes) { default_client_attributes }
    let(:required_attributes) { default_client_attributes.except('admin').except('private_key') }

    after :each do
      begin
        delete_client(admin_requestor, client_name)
      rescue URI::InvalidURIError
        # ok, since some bad names can result in bad URLs
      end
    end

    context 'when validating' do
      let(:client_name) { test_client }
      let(:test_client) { "pedant_test_#{rand(100000)}" }

      should_create_public_key
    end

    context 'valid requests of various types to create a client' do
      context 'with a "normal" non-admin client payload' do
        it 'creates a new non-admin client' do
          should look_like create_client_success_response

          # Ensure it is a non-admin
          get(client_url, requestor) do |response|
            parse(response)["admin"].should be(false)
          end
        end
      end

      context 'with a "normal" admin client payload', :smoke do
        let(:client_is_admin){true}
        it 'creates a new admin client' do
          should look_like create_client_success_response

          # Ensure it is an admin
          get(client_url, requestor) do |response|
            parse(response)["admin"].should be(true)
          end
        end
      end

      context 'with a valid name' do
        ['pedanttestingclient', 'pedanttestingclient123', 'pedant_testing_client', 'pedant.testing.client'].each do |n|
          context "like '#{n}'" do
            let(:client_name){n}

            it { should look_like create_client_success_response }
          end
        end
      end # valid names

      context 'without an admin flag' do
        let(:request_payload) do {"name" => client_name} end
        it 'succeeds, and a non-admin client is created' do
          request_payload.should_not have_key 'admin'
          should look_like create_client_success_response

          # Ensure it's not an admin
          get(client_url, requestor) do |response|
            parse(response)["admin"].should be(false)
          end
        end
      end
    end

    context 'invalid requests of various types to create a client', :validation do
      context 'with an invalid name' do
        ['pedant$testing$client', 'pedant testing client', 'pedant{testing}client'].each do |n|
          context "like '#{n}'" do
            let(:client_name){n}
            it 'fails' do
              should look_like create_client_bad_name_failure_response
            end
          end
        end
      end # invalid names

      context 'with a non-boolean admin flag' do
        let(:request_payload) do {"name" => client_name, "admin" => "sure, why not?"} end
        it 'fails' do
          should look_like create_client_failure_response
        end
      end

      context 'with an empty payload' do
        let(:request_payload){{}}
        it 'fails' do
          should look_like create_client_no_name_failure_response
        end
      end

      context 'with no name' do
        let(:request_payload) {{"admin" => false}}
        it 'fails' do
          should look_like create_client_no_name_failure_response
        end
      end
    end

    context 'creation of an existing client' do
      include_context 'with temporary testing client'

      it { should look_like create_client_conflict_response }
    end

    context 'as different kinds of clients', :authorization do
      def self.should_create_client_when(_options = {})
        context "when creating #{client_type(_options)} client" do
          let(:expected_response) { created_response }
          let(:request_payload) { client_attributes }
          let(:client_attributes) { {"name" => client_name, "admin" => _options[:admin] || false, 'validator' => _options[:validator] || false} }
          let(:success_message) do
            new_client(client_name).
              merge(client_attributes).
              with('public_key', expected_public_key)
          end

          should_respond_with 201 do
            # The new client can be retrieved (using admin_requestor
            # because validators can't retrieve clients!)
            get(client_url, admin_requestor).should look_like ok_exact_response
          end
        end
      end

      def self.should_not_create_client_when(_options = {})
        context "when creating #{client_type(_options)} client" do
          # This is really a 403 Forbidden
          let(:expected_response) { open_source_not_allowed_response }
          let(:request_payload) { { "name" => client_name, "admin" => _options[:admin] || false, 'validator' => _options[:validator] || false } }

          should_respond_with 403 do
            # Nothing new should have been created (using
            # admin_requestor because non-admin clients can't
            # retrieve any client but themselves)
            get(client_url, admin_requestor).should look_like not_found_response
          end
        end
      end

      def self.invalid_client_when(_options = {})
        context "when creating #{client_type(_options)} client" do
          let(:expected_response) { bad_request_exact_response }
          let(:error_message) { [ "Client can be either an admin or a validator, but not both." ] }
          let(:request_payload) { { "name" => client_name, "admin" => _options[:admin], 'validator' => _options[:validator] } }

          should_respond_with 400 do
            # Nothing new should have been created (using
            # admin_requestor because non-admin clients can't
            # retrieve any client but themselves)
            get(client_url, admin_requestor).should look_like not_found_response
          end
        end
      end

      # Admins can create any valid client
      as_an_admin_requestor do
        should_create_client_when admin: false, validator: false
        should_create_client_when admin: true
        should_create_client_when validator: true

        # A client that is both an admin and a validator is invalid
        invalid_client_when admin: true, validator: true
      end

      # Non-admins should not be able to create clients, period
      as_a_normal_requestor do
        should_not_create_client_when admin: false, validator: false
        should_not_create_client_when admin: true
        should_not_create_client_when validator: true

        invalid_client_when admin: true, validator: true
      end

      # Validators can only create non-admins
      context 'as a validator client' do
        let(:requestor) { validator_client }

        should_create_client_when     admin: false, validator: false
        should_not_create_client_when admin: true
        should_not_create_client_when validator: true

        invalid_client_when admin: true, validator: true
      end
    end

    respects_maximum_payload_size

  end

  should_not_allow_method :PUT,    '/clients'
  should_not_allow_method :DELETE, '/clients'

  context 'GET /clients/<name>' do
    let(:request_method) { :GET }
    let(:request_url)    { named_client_url }

    context 'with Pedant-created clients' do
      context 'the Pedant admin client', :smoke  do
        let(:client_name) { pedant_admin_client_name }
        it { should look_like fetch_admin_client_success_response }
      end
      context 'the Pedant non-admin client', :smoke do
        let(:client_name) { pedant_nonadmin_client_name }
        it { should look_like fetch_nonadmin_client_success_response }
      end

      context 'the Pedant validator client' do
        let(:client_name) { platform.validator_client_name }
        it { should look_like fetch_validator_client_success_response }
      end
    end

    context 'without an existing client' do
      let(:request_url) { api_url "/clients/#{pedant_nonexistent_client_name}" }
      it { should look_like not_found_response }
    end

    context 'with an existing client' do
      include_context 'with temporary testing client'

      def self.should_fetch_client
        it { should look_like ok_response.with(body: { 'name' => client_name }) }
      end

      def self.forbids_fetching
        it('forbids fetching', :authorization) { should look_like forbidden_response }
      end

      # Admins can fetch anyone
      as_an_admin_requestor do
        with_another_admin_client     { should_fetch_client }
        with_another_validator_client { should_fetch_client }
        with_another_normal_client    { should_fetch_client }

        with_self do
          let(:client_is_admin) { true }
          should_fetch_client
        end
      end

      # Validator clients can only fetch themselves
      context 'as a validator client' do
        let(:requestor) { validator_client}

        with_another_admin_client     { forbids_fetching }
        with_another_validator_client { forbids_fetching }
        with_another_normal_client    { forbids_fetching }

        with_self do
          let(:client_is_validator) { true }
          should_fetch_client
        end
      end

      # Normal clients can only fetch themselves
      context 'as a normal client' do
        let(:requestor) { normal_client }

        with_another_admin_client     { forbids_fetching }
        with_another_validator_client { forbids_fetching }
        with_another_normal_client    { forbids_fetching }
        with_self                     { should_fetch_client }
      end

      # Normal users cannot fetch any clients
      context 'as a normal user' do
        let(:requestor) { normal_user }

        with_another_admin_client     { forbids_fetching }
        with_another_validator_client { forbids_fetching }
        with_another_normal_client    { forbids_fetching }
      end
    end
  end

  should_not_allow_method :POST, '/clients/pedant_test_client'

  context 'PUT /clients/<name>' do
    include Pedant::RSpec::Validations::Update

    let(:request_method)  { :PUT }
    let(:request_url)     { named_client_url }
    let(:request_payload) { default_client_attributes }

    include_context "with temporary testing client"
    let(:expected_response)   { ok_response }
    let(:resource_url)        { client_url }
    let(:required_attributes) { required_client_attributes }

    # Smoke test
    context 'as an admin' do
      let(:requestor) { platform.admin_client }

      context 'with admin set to true', :smoke do
        let(:request_payload) { required_attributes.with('admin', true) }

        it { should look_like ok_response }
      end
    end

    context 'modifying a non-existent client' do
      let(:request_url) { api_url "/clients/#{pedant_nonexistent_client_name}" }
      let(:request_payload) { {"name" => pedant_nonexistent_client_name} }

      it { should look_like client_not_found_response }
    end

    def self.should_update_client_when(_options = {})
      context "when updating to #{client_type(_options)} client" do
        let(:expected_response) { ok_response }
        let(:request_payload) { client_attributes }

        #let(:client_attributes) { {"name" => client_name, "admin" => _options[:admin] || false, 'validator' => _options[:validator] || false} }
        # Test default values
        let(:client_attributes) do
          {"name" => client_name }.tap do |h|
            h['admin']     = _options[:admin]     unless _options[:admin].nil?
            h['validator'] = _options[:validator] unless _options[:validator].nil?
          end
        end

        let(:success_message) do
          new_client(client_name).
            merge(client_attributes).
            with('public_key', expected_public_key)
        end

        should_respond_with 200 do
          # The new client can be retrieved (using admin_requestor
          # because validators can't retrieve clients!)
          get(client_url, platform.admin_user).should look_like ok_exact_response
        end
      end
    end

    def self.forbids_update_when(_options = {})
      context "when updating to #{client_type(_options)} client" do
        # This is really a 403 Forbidden
        let(:expected_response) { forbidden_response }
        let(:request_payload) { { "name" => client_name, "admin" => _options[:admin] || false, 'validator' => _options[:validator] || false } }

        should_respond_with 403 do
          # Nothing new should have been created (using
          # admin_requestor because non-admin clients can't
          # retrieve any client but themselves)
          get(client_url, platform.admin_user).should look_like original_resource_attributes
        end
      end
    end

    def self.forbids_renaming
      context 'when renaming client' do
        let(:request_payload) { { 'name' => new_name } }
        let(:persisted_renamed_client_response) { get renamed_client_url, admin_user }
        let(:original_client_response) { persisted_resource_response }
        let(:renamed_client_url) { api_url "/clients/#{new_name}" }
        let(:renamed_client_attributes) { original_resource_attributes.with('name', new_name) }

        context 'to an unclaimed name' do
          let(:expected_response) { forbidden_response }
          #TODO: Test the exact response body

          let(:new_name) { "#{client_name}_new" }

          should_respond_with 403, 'and does not rename the client' do
            original_client_response.should look_like ok_response
          end
        end

        context 'to an existing name' do
          let(:expected_response) { forbidden_response }
          let(:new_name) { normal_client.name }

          should_respond_with 403, 'and does not rename the client' do
            original_client_response.should look_like ok_response
          end
        end
      end # when renaming client
    end # should rename client

    def self.should_rename_client
      context 'when renaming client' do
        let(:request_payload) { { 'name' => new_name } }
        let(:persisted_renamed_client_response) { get renamed_client_url, admin_user }
        let(:original_client_response) { persisted_resource_response }
        let(:renamed_client_url) { api_url "/clients/#{new_name}" }
        let(:renamed_client_attributes) { original_resource_attributes.with('name', new_name) }

        # TODO: This test will probably break legacy code that uses couchdb

        context 'to an unclaimed name' do
          let(:expected_response) { created_response } # Not sure why renames create a new resource
          #TODO: Test the exact response body

          after(:each) { delete_client admin_user, new_name }
          let(:new_name) { "#{client_name}_new" }

          should_respond_with 201, 'and rename the client' do
            original_client_response.should look_like not_found_response
            persisted_renamed_client_response.should look_like ok_response.with('body_exact', renamed_client_attributes)
          end
        end

        context 'to an existing name' do
          let(:expected_response) { conflict_response }
          let(:new_name) { normal_client.name }

          should_respond_with 409, 'and does not rename the client' do
            original_client_response.should look_like ok_response
          end
        end
      end # when renaming client
    end # should rename client

    def self.invalid_client_when(_options = {})
      context "when updating to #{client_type(_options)} client" do
        let(:expected_response) { bad_request_exact_response }
        let(:error_message) { [ "Client can be either an admin or a validator, but not both." ] }
        let(:request_payload) { { "name" => client_name, "admin" => _options[:admin], 'validator' => _options[:validator] } }

        should_respond_with 400 do
           get(client_url, admin_requestor).should look_like original_resource_attributes
        end
      end
    end

    # Admins can do anything
    as_an_admin_requestor do
      with_another_admin_client do
        should_update_client_when admin: false
        should_update_client_when admin: true
        should_update_client_when admin: false, validator: true
        invalid_client_when       admin: true,  validator: true

        should_rename_client
        should_generate_new_keys
        should_update_public_key
      end

      with_another_validator_client do
        should_update_client_when validator: false
        should_update_client_when validator: true
        should_update_client_when validator: false, admin: true
        invalid_client_when       admin: true, validator: true

        should_rename_client
        should_generate_new_keys
        should_update_public_key
      end

      with_another_normal_client do
        should_update_client_when admin: false, validator: false
        should_update_client_when admin: true
        should_update_client_when validator: true
        invalid_client_when       admin: true, validator: true

        should_rename_client
        should_generate_new_keys
        should_update_public_key
      end
    end

    context 'as an admin client' do
      let(:requestor) { admin_client }

      with_self do
        let(:client_is_admin) { true } # Self is an admin

        should_update_client_when admin: false
        should_update_client_when admin: true
        should_update_client_when admin: false, validator: true
        invalid_client_when       admin: true,  validator: true

        should_rename_client
        should_generate_new_keys
        should_update_public_key
      end
    end

    # Validator clients can only create clients or update self
    context 'as a validator client' do
      let(:requestor) { validator_client }

      with_another_admin_client do
        forbids_update_when admin: false
        forbids_update_when admin: false, validator: true
        invalid_client_when admin: true,  validator: true

        forbids_renaming
      end

      with_another_validator_client do
        forbids_update_when validator: false
        forbids_update_when validator: false, admin: true
        invalid_client_when admin: true, validator: true

        forbids_renaming
      end

      with_another_normal_client do
        forbids_update_when admin: false, validator: false
        forbids_update_when admin: true
        forbids_update_when validator: true
        invalid_client_when admin: true, validator: true

        forbids_renaming
      end

      with_self do
        let(:client_is_validator) { true } # Self is a validator

        # Validator can say it is a validator
        should_update_client_when validator: true

        # Validator can downgrade to a normal client
        should_update_client_when validator: false

        # Validators cannot upgrade themselves to an admin
        forbids_update_when validator: false, admin: true
        invalid_client_when admin: true, validator: true

        # Otherwise, validators can update themselves
        should_rename_client
        should_generate_new_keys
        should_update_public_key
      end
    end

    # Normal clients can only update self
    context 'as a normal client' do
      let(:requestor) { normal_client }

      with_another_admin_client do
        forbids_update_when admin: false
        forbids_update_when admin: false, validator: true
        invalid_client_when admin: true,  validator: true

        forbids_renaming
      end

      with_another_validator_client do
        forbids_update_when validator: false
        forbids_update_when validator: false, admin: true
        invalid_client_when admin: true, validator: true

        forbids_renaming
      end

      with_another_normal_client do
        forbids_update_when admin: false, validator: false
        forbids_update_when admin: true
        forbids_update_when validator: true
        invalid_client_when admin: true, validator: true

        forbids_renaming
      end

      with_self do
        # Self is normal
        let(:client_is_admin)     { false }
        let(:client_is_validator) { false }

        # Normal users can say they are not an admin or validator
        should_update_client_when admin: false, validator: false

        # Normal users cannot upgrade themselves
        forbids_update_when admin: true
        forbids_update_when validator: true
        invalid_client_when admin: true, validator: true

        # Otherwise, normal users can update themselves
        should_rename_client
        should_generate_new_keys
        should_update_public_key
      end

      # This give back 401 Unauthorized
      skip "with a normal user of the same name as the client" do
        let(:requestor) { user_requestor }
        after(:each)  { delete api_url("/users/#{client_name}"), admin_user }

        let(:user_requestor) { Pedant::User.new(client_name, user_private_key, platform: platform, preexisting: false) }
        let(:user_response) { post api_url('/users'), superuser, payload: user_attributes }
        let(:user_parsed_response) { parse(user_response).tap { |x| puts x } }
        let(:user_private_key) { user_parsed_response['private_key'] }
        let(:user_attributes) do
          {
            "name" => client_name,
            "password" => SecureRandom.hex(32),
            "admin" => false
          }
        end

        forbids_renaming
      end

    end

    respects_maximum_payload_size
  end

  context 'DELETE /clients/<name>' do
    let(:request_method)  { :DELETE }
    let(:request_url)     { named_client_url }

    include_context 'with temporary testing client'

    def self.should_delete_client
      it { should look_like ok_response.with(body: { 'name' => client_name }) }
    end

    def self.forbids_deletion
      it('forbids deletion', :authorization) { should look_like forbidden_response }
    end

    context 'without an existing client' do
      let(:requestor) { admin_requestor }
      let(:request_url) { api_url "/clients/#{pedant_nonexistent_client_name}" }

      it { should look_like client_not_found_response }
    end

    # Admins can delete any client
    as_an_admin_requestor do
      with_another_admin_client     { should_delete_client }
      with_another_validator_client { should_delete_client }
      with_another_normal_client    { should_delete_client }

      with_self do
        let(:client_is_admin) { true }
        should_delete_client
      end
    end

    # Validators can only delete itself
    context 'as a validator client' do
      let(:requestor) { validator_client }

      with_another_admin_client     { forbids_deletion }
      with_another_validator_client { forbids_deletion }
      with_another_normal_client    { forbids_deletion }

      with_self do
        let(:client_is_validator) { true }
        should_delete_client
      end
    end

    # Normal clients can only delete itself
    context 'as a normal client' do
      let(:requestor) { normal_client }

      with_another_admin_client     { forbids_deletion }
      with_another_validator_client { forbids_deletion }
      with_another_normal_client    { forbids_deletion }
      with_self                     { should_delete_client }
    end
  end
end
