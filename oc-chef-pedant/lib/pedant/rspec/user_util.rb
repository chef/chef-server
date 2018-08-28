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

require 'pedant/json'
require 'pedant/request'
require 'rspec/core/shared_context'
require 'openssl'

module Pedant
  module RSpec
    module UserUtil
      extend ::RSpec::Core::SharedContext
      extend ::Pedant::Concern

      include Pedant::JSON
      include Pedant::Request

      let(:user_collection_url) { api_url "/users" }
      let(:authenticate_user_url) { api_url "/authenticate_user" }
      let(:default_user_password) { 'opensesame123' }
      let(:default_user_attributes) do
          {
            "username" => default_user_name,
            "password" => default_user_password,
            "admin" => true
          }
      end

      def create_user(attributes, requestor)
        post(user_collection_url, requestor, payload: attributes)
      end

      def authenticate_user(name, password)
        r = parse(post(authenticate_user_url, superuser, payload: { 'name' => name, 'password' => password }))
        fail 'Something went wrong while authenticating a user' if r['verified'].nil?
        r['verified']
      end

      def delete_user(user_name, requestor = superuser)
        delete(api_url("/users/#{user_name}"), requestor)
      end


      # TODO: are we leaving behind a bunch of group cruft in CouchDB each
      # time we run a test?  Check out the groups of the org!

      def group_in_org(group_name)
        response = get(api_url("/groups/#{group_name}"), superuser)
        response.code == 200
      end

      def member_of_group(user, group_name)
        response = get(api_url("/groups/#{group_name}"), superuser)
        group_info = parse(response)
        group_info["actors"].include?(user.name)
      end

      def associated_with_org(user)
        response = get(api_url("/users"), superuser)
        user_info = parse(response)

        # here's what should come back:
        #  [{"user"=>{"username"=>"pedant-admin"}},
        #   {"user"=>{"username"=>"pedant-normal"}}]
        #

        user_info.any? do |user_item|
          user_item["user"]["username"] == user.name
        end

      end

      module ClassMethods

        def validates_name
          context "when validating 'name' field" do
            let(:validate_attribute) { 'name' }
            validates_existence_of 'name'

            rejects_invalid_value 'USERNAME'
            rejects_invalid_value 'user@example.org'
            rejects_invalid_value 'user name'
            rejects_invalid_value 'user|name'
          end
        end

        def validates_admin_flag
          context "when validating 'admin' field" do
            let(:validate_attribute) { 'admin' }
            optionally_accepts_value true, default: false
            optionally_accepts_value false, default: false

            rejects_invalid_value 'random string'
            rejects_invalid_value []
            rejects_invalid_value Hash.new
            rejects_invalid_value 1
          end
        end

        def should_generate_new_keys
          context 'when generating key pairs' do
            let(:updated_private_key) { parsed_response['private_key'] }
            let(:updated_response) { http_200_response.with(:body, updated_resource) }

            context 'with private_key set to true' do
              let(:request_payload) { required_attributes.with('private_key', true) }
              let(:new_public_key) { parsed_response['public_key'] }
              let(:updated_resource) { required_attributes.with('public_key', updated_public_key) }
              let(:updated_public_key) { parsed_response['public_key'] }
              let(:updated_requestor) { Pedant::User.new(test_user, updated_private_key, platform: platform, preexisting: false) }

              should_respond_with 200, 'and generate a new key pair' do
                updated_private_key.should_not be_nil
                updated_public_key.should_not be_nil

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)
                authenticate_user(default_user_name, default_user_password).should be(true)

                # Now verify we can use the new credentials
                get(resource_url, updated_requestor).should look_like updated_response
              end
            end # when private_key is true

            context 'with private_key set to false' do
              let(:request_payload) { required_attributes.with('private_key', false) }
              let(:updated_resource) { required_attributes.with('public_key', test_user_public_key) }

              should_not_generate_new_key_pair
            end # when private_key is false

            context 'without a private_key' do
              let(:request_payload) { required_attributes.except('private_key') }
              let(:updated_resource) { required_attributes.with('public_key', test_user_public_key) }

              should_not_generate_new_key_pair
            end # when private_key is nil

            rejects_invalid_private_key_flag nil
            rejects_invalid_private_key_flag 1
            rejects_invalid_private_key_flag ""
            rejects_invalid_private_key_flag []
            rejects_invalid_private_key_flag Hash.new

          end # when generating key pairs
        end

        # Private macro
        def should_not_generate_new_key_pair
          should_respond_with 200, 'and does not generate a new key pair' do
            parsed_response['private_key'].should be(nil)

            # Now verify that you can retrieve it again
            persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)
            authenticate_user(default_user_name, default_user_password).should be(true)

            # Now verify we can use the original credentials
            get(resource_url, test_user_requestor).should look_like updated_response
          end
        end

        # Private macro
        def rejects_invalid_private_key_flag(value)
          context "with private_key set to #{value.inspect}" do
            let(:expected_response) { bad_request_response }
            let(:request_payload) { required_attributes.with('private_key', value) }
            let(:updated_resource) { required_attributes.with('public_key', test_user_public_key) }

            should_respond_with 400, 'and does not generate a new key pair' do
              parsed_response['private_key'].should be(nil)

              # Now verify that you can retrieve it again
              persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)
              authenticate_user(default_user_name, default_user_password).should be(true)

              # Now verify we can use the original credentials
              get(resource_url, test_user_requestor).should look_like updated_response
            end
          end # when private_key is nil
        end

        def should_create_public_key
          context 'when setting public_key' do
            let(:request_payload) { required_attributes.with('public_key', public_key) }
            let(:updated_resource) { required_attributes.with('public_key', public_key).except('password') }
            let(:private_key) { OpenSSL::PKey::RSA.new(2048) }
            let(:public_key) { private_key.public_key.to_s }
            let(:created_requestor) { Pedant::User.new(test_user, private_key, platform: platform, preexisting: false) }
            let(:updated_response) { http_200_response.with(:body, updated_resource) }

            let(:created_public_key) { parsed_response['public_key'] }
            let(:created_private_key) { parsed_response['private_key'] }


            should_respond_with 201, 'and create the user' do
              parsed_response['public_key'].should_not be_nil
              parsed_response.member?('private_key').should be(false) # Make sure private_key is not returned at all

              # Now verify that you can retrieve it again
              persisted_resource_response.should look_like updated_response
              authenticate_user(default_user_name, default_user_password).should be(true)

              # Verify that we can use the new credentials
              get(resource_url, created_requestor).should look_like updated_response
            end

            context 'without a public key' do
              let(:request_payload) { required_attributes }
              let(:private_key) { created_private_key }
              let(:public_key) { created_public_key }

              should_respond_with 200, 'and generates a new keypair' do
                created_public_key.should_not be_nil
                created_private_key.should_not be_nil

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response
                authenticate_user(default_user_name, default_user_password).should be(true)

                # Verify that we can use the new credentials
                get(resource_url, created_requestor).should look_like updated_response
              end
            end

            context 'with nil for the public key' do
              let(:public_key) { nil }
              let(:private_key) { created_private_key }

              let(:updated_resource) { required_attributes.with('public_key', created_public_key).except('password') }

              should_respond_with 200, 'and generates a new keypair' do
                created_public_key.should_not be_nil
                created_private_key.should_not be_nil

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response
                authenticate_user(default_user_name, default_user_password).should be(true)

                # Verify that we can use the new credentials
                get(resource_url, created_requestor).should look_like updated_response
              end
            end

            context 'with a bad public_key' do
              # Use the original public key
              #let(:updated_resource) { required_attributes.with('public_key', public_key) }

              rejects_public_key_on_create_with "well-formed, bogus", public_key: Proc.new { bogus_key }
              rejects_public_key_on_create_with "mal-formed", public_key: "-----BEGIN PUBLIC KEY-----You have been trolled :-)-----END PUBLIC KEY-----"
              rejects_public_key_on_create_with "mal-formed RSA", public_key: "-----BEGIN RSA PUBLIC KEY-----You have been trolled :-)-----END RSA PUBLIC KEY-----"
              rejects_public_key_on_create_with "mal-formed cert", public_key: "-----BEGIN CERTIFICATE-----You have been trolled :-)-----END CERTIFICATE-----"
              rejects_public_key_on_create_with "blank", public_key: ""

              # Invalid JSON types
              rejects_public_key_on_create_with "1 for the",  public_key: 1
              rejects_public_key_on_create_with "[] for the", public_key: []
              rejects_public_key_on_create_with "{} for the", public_key: {}
            end

          end # when setting private_key to true
        end


        def should_update_public_key
          context 'when updating public_key' do
            let(:request_payload) { required_attributes.with('public_key', public_key) }
            let(:updated_resource) { required_attributes.with('public_key', public_key) }
            let(:private_key) { OpenSSL::PKey::RSA.new(2048) }
            let(:public_key) { private_key.public_key.to_s }
            let(:updated_requestor) { Pedant::User.new(test_user, private_key, platform: platform, preexisting: false) }
            let(:updated_response) { http_200_response.with(:body, updated_resource) }

            should_respond_with 200, 'and update the public key' do
              parsed_response['public_key'].should_not be_nil
              parsed_response.member?('private_key').should be(false) # Make sure private_key is not returned at all

              # Now verify that you can retrieve it again
              persisted_resource_response.should look_like updated_response
              authenticate_user(default_user_name, default_user_password).should be(true)

              # Verify that we can use the new credentials
              get(resource_url, updated_requestor).should look_like updated_response
            end

            context 'without a public key' do
              let(:request_payload) { required_attributes }

              # Use the original public key
              let(:updated_resource) { required_attributes.with('public_key', test_user_public_key) }

              should_respond_with 200, 'and does not update the public key' do
                parsed_response['public_key'].should be_nil # We did not update the public key, so this should not be set
                parsed_response.member?('private_key').should be(false) # Make sure private_key is not returned at all

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response
                authenticate_user(default_user_name, default_user_password).should be(true)

                # Verify that we can use the new credentials
                get(resource_url, test_user_requestor).should look_like updated_response
              end
            end

            # PATCHy public_key that also accepts nil
            context 'with nil for the public key' do
              let(:public_key) { nil }

              # Use the original public key
              let(:updated_resource) { required_attributes.with('public_key', test_user_public_key) }

              should_respond_with 200, 'and does not update the public key' do
                parsed_response['public_key'].should be_nil # We did not update the public key, so this should not be set
                parsed_response.member?('private_key').should be(false) # Make sure private_key is not returned at all

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response
                authenticate_user(default_user_name, default_user_password).should be(true)

                # Verify that we can use the new credentials
                get(resource_url, test_user_requestor).should look_like updated_response
              end
            end

            context 'with a bad public_key' do
              # Use the original public key
              let(:updated_resource) { required_attributes.with('public_key', test_user_public_key) }

              rejects_public_key_on_update_with "well-formed, bogus", public_key: Proc.new { bogus_key }
              rejects_public_key_on_update_with "mal-formed", public_key: "-----BEGIN PUBLIC KEY-----You have been trolled :-)-----END PUBLIC KEY-----"
              rejects_public_key_on_update_with "mal-formed RSA", public_key: "-----BEGIN RSA PUBLIC KEY-----You have been trolled :-)-----END RSA PUBLIC KEY-----"
              rejects_public_key_on_update_with "mal-formed cert", public_key: "-----BEGIN CERTIFICATE-----You have been trolled :-)-----END CERTIFICATE-----"
              rejects_public_key_on_update_with "blank", public_key: ""

              # Invalid JSON types
              rejects_public_key_on_update_with "1 for the",  public_key: 1
              rejects_public_key_on_update_with "[] for the", public_key: []
              rejects_public_key_on_update_with "{} for the", public_key: {}
            end
          end # when updating the public key
        end

        def rejects_public_key_on_create_with(adjective, _options = {})
          context "with a #{adjective} public key" do
            let(:public_key) { instance_eval_if_proc(_options[:public_key]) }
            let(:expected_response) { bad_request_response }

            should_respond_with 400, 'and does not create the user' do
              # Make sure the resource has not been persisted
              persisted_resource_response.should look_like not_found_response

              # Make sure we cannot use this credential
              authenticate_user(default_user_name, default_user_password).should be(false)
              get(resource_url, created_requestor).should look_like unauthorized_response
            end
          end
        end

        def rejects_public_key_on_update_with(adjective, _options = {})
          context "with a #{adjective} public key" do
            let(:public_key) { instance_eval_if_proc(_options[:public_key]) }
            let(:expected_response) { bad_request_response }

            should_respond_with 400, 'and does not update the user' do
              # Verify nothing has changed
              persisted_resource_response.should look_like updated_response
              authenticate_user(default_user_name, default_user_password).should be(true)

              # Verify that we can use the original credentials
              get(resource_url, test_user_requestor).should look_like updated_response
            end
          end
        end

        def should_update_without_password
          context 'without a password' do
            let(:request_payload) { required_attributes }
            let(:default_resource_attributes) { default_user_attributes }
            let(:updated_resource) { required_attributes }

            should_respond_with 200, 'and preserve the password' do
              parsed_response['password'].should be_nil
              parsed_response['salt'].should be_nil

              # Now verify that you can retrieve it again
              persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)
              authenticate_user(default_user_name, default_user_password).should be(true)
            end
          end # when setting a password
        end

        def should_update_password
          context 'when setting a password' do
            let(:new_password) { SecureRandom.hex(10) }
            let(:request_payload) { required_attributes.with('password', new_password) }
            let(:default_resource_attributes) { default_user_attributes }
            let(:new_public_key) { parsed_response['public_key'] }
            let(:updated_resource) { required_attributes }

            should_respond_with 200, 'and update the user' do
              parsed_response['password'].should be_nil
              parsed_response['salt'].should be_nil

              # Now verify that you can retrieve it again
              persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)
              authenticate_user(default_user_name, new_password).should be(true)
            end
          end # when setting a password
        end
      end # ClassMethods

    end # UserUtil
  end # RSpec
end # Pedant
