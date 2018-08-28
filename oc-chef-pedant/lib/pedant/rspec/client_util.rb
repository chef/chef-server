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

require 'pedant/request'
require 'rspec/core/shared_context'

module Pedant
  module RSpec
    module ClientUtil
      extend ::RSpec::Core::SharedContext
      extend Pedant::Concern

      # When you include this context, 'client_name' is set to the
      # name of the testing client
      shared_context 'with temporary testing client' do
        before(:each) { test_client_response }
        after :each do
          begin
            delete_client(platform.admin_user, client_name)
          rescue URI::InvalidURIError
            # ok, since some bad names can result in bad URLs
          end
        end

        let(:client_name)         { unique_name("temporary_client") }
        let(:client_is_admin)     { false }
        let(:client_is_validator) { false }

        let(:client) { default_client_attributes } # Compatibility with search tests

        let(:required_client_attributes) { { 'name' => client_name } }
        let(:default_client_attributes) do
          attrs = required_client_attributes.with('validator', client_is_validator)
          platform.server_api_version > 0 ? attrs.with('create_key', true) : attrs
        end
        let(:original_resource_attributes) { default_client_attributes.except('private_key') }

        let(:test_client) { client_name }
        let(:test_client_response) { create_client admin_requestor, default_resource_attributes }
        let(:test_client_parsed_response) { parse(test_client_response) }
        let(:test_client_private_key) do
          platform.server_api_version > 0 ? test_client_parsed_response['chef_key']['private_key'] : test_client_parsed_response['private_key']
        end
        let(:test_client_public_key) do
          platform.server_api_version > 0 ? test_client_parsed_response['chef_key']['public_key'] : test_client_parsed_response['public_key']
        end
        let(:test_client_requestor) { Pedant::Client.new(test_client, test_client_private_key, platform: platform, preexisting: false) }

        let(:client_url) { api_url("/clients/#{client_name}") }

        let(:persisted_resource_response) { get(resource_url, platform.admin_user) }
        let(:default_resource_attributes) { default_client_attributes }
      end # shared context

      let(:pedant_admin_client_name){Pedant.config.requestors[:clients][:admin][:name]}
      let(:pedant_nonexistent_client_name){"non-existent"}

      # These will be used all over the place
      let(:clients_url){api_url("/clients")}
      let(:client_name) { fail "Please specify a 'client_name' first" }
      let(:named_client_url){api_url("/clients/#{client_name}")}

      let(:client_not_found_response) { resource_not_found_response }

      let(:expected_public_key) { /^-----BEGIN (RSA )?PUBLIC KEY/ }
      let(:expected_private_key) { /^-----BEGIN (RSA )?PRIVATE KEY/ }
      let(:fetch_validator_client_success_response)  do
        if platform.server_api_version > 0
          ok_response.with(body_exact: new_client(client_name, validator: true).with('public_key', expected_public_key))
        else
          ok_response.with(body_exact: new_client(client_name, validator: true))
        end
      end

      let(:delete_client_success_response) { ok_response.with(body: { 'name' => client_name }) }

      let(:create_client_success_response) do
        if platform.server_api_version > 0
          {
            :status => 201,
            :body_exact => {
              "uri" => named_client_url,
              chef_key => {
                "name" => "default",
                "expiration_date" => "infinity",
                "uri" => "#{named_client_url}/keys/default",
                "private_key" => expected_private_key,
                "public_key" => expected_public_key
              }
            }
          }
        else
          {

            :status => 201,
            :body_exact => {
              "uri" => named_client_url,
              "private_key" => expected_private_key,
              "public_key" => expected_public_key
            }
        }

        end
      end

      let(:create_client_bad_name_failure_response) do
        {
          :status => 400,
          :body_exact => {
            "error" => ["Invalid client name '#{client_name}' using regex: 'Malformed client name.  Must be A-Z, a-z, 0-9, _, -, or .'."]
          }
        }
      end


      let(:create_client_no_name_failure_response) do
        {
          :status => 400,
          :body_exact => {
            "error" => ["Field 'name' missing"]
          }
        }
      end

      # should this be create_client_invalid_request_response ?
      let(:create_client_failure_response) do
        {
          :status => 400
        }
      end

      let(:create_client_conflict_response) do
        {
          :status => 409,
          :body_exact => {
            "error" => ["Client already exists"]
          }
        }
      end

      def new_client(name, _options = {})
        _options[:validator] ||= false
        {
          "name" => name,
          "clientname" => name,
          "chef_type" => "client",
          "orgname" => platform.org_name,
          "json_class" => "Chef::ApiClient",
          "validator" => _options[:validator],
        }
      end

      def add_client(requestor, client)
        post(api_url("/clients"), requestor, :payload => client)
      end

      def create_client(requestor, client)
        add_client(requestor, client)
      end

      def delete_client(requestor, client_name)
        delete(api_url("/clients/#{client_name}"), requestor)
      end

      module ClassMethods
        def with_another_validator_client(&examples)
          context 'with another validator client' do
            let(:client_is_validator) { true }
            instance_eval(&examples)
          end
        end

        def with_self(&examples)
          context 'with self' do
            let(:requestor) { test_client_requestor }
            instance_eval(&examples)
          end
        end

        def with_another_normal_client(&examples)
          context('with another normal client', &examples)
        end

        def client_type(_options)
          _options[:validator] ? "a validator" : "a normal"
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
              let(:updated_requestor) { Pedant::User.new(test_client, updated_private_key, platform: platform, preexisting: false) }

              should_respond_with 200, 'and generate a new key pair' do
                updated_private_key.should_not be_nil
                updated_public_key.should_not be_nil

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)

                # Now verify we can use the new credentials
                get(resource_url, updated_requestor).should look_like updated_response
              end
            end # when private_key is true

            context 'with private_key set to false' do
              let(:request_payload) { required_attributes.with('private_key', false) }
              let(:updated_resource) { required_attributes.with('public_key', test_client_public_key) }

              should_not_generate_new_key_pair
            end # when private_key is false

            context 'without a private_key' do
              let(:request_payload) { required_attributes.except('private_key') }
              let(:updated_resource) { required_attributes.with('public_key', test_client_public_key) }

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
            parsed_response['private_key'].should be(false)

            # Now verify that you can retrieve it again
            persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)

            # Now verify we can use the original credentials
            get(resource_url, test_client_requestor).should look_like updated_response
          end
        end
        #
        # Private macro
        def rejects_invalid_private_key_flag(value)
          context "with private_key set to #{value.inspect}", :validation do
            let(:requestor) { platform.admin_user }
            let(:expected_response) { bad_request_response }
            let(:request_payload) { required_attributes.with('private_key', value) }
            let(:updated_resource) { required_attributes.with('public_key', test_client_public_key) }

            should_respond_with 400, 'and does not generate a new key pair' do
              parsed_response['private_key'].should be(nil)

              persisted_resource_response.should look_like http_200_response.with(:body, updated_resource)
              # Now verify that you can retrieve it again

              # Now verify we can use the original credentials
              get(resource_url, test_client_requestor).should look_like updated_response
            end
          end # when private_key is nil
        end

        def should_create_public_key
          context 'when setting public_key' do
            let(:request_payload) { required_attributes.with('public_key', public_key) }
            let(:updated_resource) { required_attributes.with('public_key', public_key).except('password') }
            let(:private_key) { OpenSSL::PKey::RSA.new(2048) }
            let(:public_key) { private_key.public_key.to_s }
            let(:created_requestor) { Pedant::User.new(test_client, private_key, platform: platform, preexisting: false) }
            let(:updated_response) { http_200_response.with(:body, updated_resource) }

            let(:created_public_key) { parsed_response['public_key'] }
            let(:created_private_key) { parsed_response['private_key'] }

            should_respond_with 201, 'and create the client' do
              parsed_response['public_key'].should_not be_nil
              parsed_response.member?('private_key').should be(false) # Make sure private_key is not returned at all

              # Now verify that you can retrieve it again
              persisted_resource_response.should look_like updated_response

              # Verify that we can use the new credentials
              get(resource_url, created_requestor).should look_like updated_response
            end

            context 'without a public key' do
              let(:request_payload) { required_attributes }
              let(:private_key) { created_private_key }
              let(:public_key) { created_public_key }

              should_respond_with 200, 'and generates a new keypair' do
                created_public_key.should =~ expected_public_key
                created_private_key.should =~ expected_private_key

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response

                # Verify that we can use the new credentials
                get(resource_url, created_requestor).should look_like updated_response
              end
            end

            # Unlike many of the PATCHy attributes, the API should accept
            # nil for a public_key and generate a key pair
            context 'with nil for a public key' do
              let(:public_key) { nil }
              let(:private_key) { created_private_key }

              let(:updated_resource) { required_attributes.with('public_key', created_public_key).except('password') }

              should_respond_with 200, 'and generates a new keypair' do
                created_public_key.should =~ expected_public_key
                created_private_key.should =~ expected_private_key

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response

                # Verify that we can use the new credentials
                get(resource_url, created_requestor).should look_like updated_response
              end
            end

            context 'with a bad public_key [v1+]', :validation, :api_v1 do
              before (:all) do
                platform.use_max_server_api_version
              end
              after (:all) do
                platform.reset_server_api_version
              end
              # Use the original public key
              #let(:updated_resource) { required_attributes.with('public_key', public_key) }

              # Public key validation is not enabled until min api version 1
              rejects_public_key_on_create_with "well-formed, bogus (private key)", public_key: Proc.new { bogus_key }
              rejects_public_key_on_create_with "mal-formed", public_key: "-----BEGIN PUBLIC KEY-----You have been trolled :-)-----END PUBLIC KEY-----"
              rejects_public_key_on_create_with "mal-formed RSA", public_key: "-----BEGIN RSA PUBLIC KEY-----You have been trolled :-)-----END RSA PUBLIC KEY-----"
              rejects_public_key_on_create_with "mal-formed cert", public_key: "-----BEGIN CERTIFICATE-----You have been trolled :-)-----END CERTIFICATE-----"
              rejects_public_key_on_create_with "blank", public_key: ""
              # certs are not accepted in a v1+ world
              #TODO rejects_public_key_on_create_with "a valid certificate", public_key: platform.valid_certificate

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
            let(:updated_requestor) { Pedant::User.new(client_name, private_key, platform: platform, preexisting: false) }
            let(:updated_response) { http_200_response.with(:body, updated_resource) }

            should_respond_with 200, 'and update the public key' do
              parsed_response['public_key'].should_not be_nil
              parsed_response['private_key'].should be(false)

              # Now verify that you can retrieve it again
              persisted_resource_response.should look_like updated_response

              # Verify that we can use the new credentials
              get(resource_url, updated_requestor).should look_like updated_response
            end

            context 'without a public key' do
              let(:request_payload) { required_attributes }

              # Use the original public key
              let(:updated_resource) { required_attributes.with('public_key', test_client_public_key) }

              should_respond_with 200, 'and does not update the public key' do
                # TODO: In users, if you do not send a public key you get nothing back
                # Since public_key is supposed to be PATCHy, then maybe we should get something back after all
                # parsed_response['public_key'].should be_nil

                # Make sure we did not change the public key
                parsed_response['public_key'].should eql test_client_public_key
                parsed_response['private_key'].should be(false)
                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response

                # Verify that we can use the new credentials
                get(resource_url, test_client_requestor).should look_like updated_response
              end
            end

            context 'with nil for the public key' do
              let(:public_key) { nil }

              # Use the original public key
              let(:updated_resource) { required_attributes.with('public_key', test_client_public_key) }

              should_respond_with 200, 'and does not update the public key' do
                # TODO: In users, if you do not send a public key you get nothing back
                # Since public_key is supposed to be PATCHy, then maybe we should get something back after all
                # parsed_response['public_key'].should be_nil

                # Make sure we did not change the public key
                parsed_response['public_key'].should eql test_client_public_key
                # Nor generate a private key
                parsed_response['private_key'].should be(false)

                # Now verify that you can retrieve it again
                persisted_resource_response.should look_like updated_response

                # Verify that we can use the new credentials
                get(resource_url, test_client_requestor).should look_like updated_response
              end
            end

            context 'with a bad public_key [v1+]', :validation, :api_v1 do
              before (:all) do
                platform.use_max_server_api_version
              end
              after (:all) do
                platform.reset_server_api_version
              end
              # Public key validation is not enabled until min api version 1
              let(:updated_resource) { required_attributes }
              rejects_public_key_on_update_with "well-formed, bogus", public_key: Proc.new { bogus_key } unless
              rejects_public_key_on_update_with "mal-formed", public_key: "-----BEGIN PUBLIC KEY-----You have been trolled :-)-----END PUBLIC KEY-----"
              rejects_public_key_on_update_with "mal-formed RSA", public_key: "-----BEGIN RSA PUBLIC KEY-----You have been trolled :-)-----END RSA PUBLIC KEY-----"
              rejects_public_key_on_update_with "mal-formed cert", public_key: "-----BEGIN CERTIFICATE-----You have been trolled :-)-----END CERTIFICATE-----"
              rejects_public_key_on_update_with "blank", public_key: ""
              # certs are not accepted in a v1+ world
              # TODO  rejects_public_key_on_update_With "a valid certificate", public_key: platform.valid_certificate

              # Invalid JSON types
              rejects_public_key_on_update_with "1 for the",  public_key: 1
              rejects_public_key_on_update_with "[] for the", public_key: []
              rejects_public_key_on_update_with "{} for the", public_key: {}
            end
          end # when updating the public key
        end

        def rejects_public_key_on_create_with(adjective, _options = {})
          context "with a #{adjective} public key" do
            let(:requestor) { platform.admin_user }
            let(:public_key) { instance_eval_if_proc(_options[:public_key]) }
            let(:expected_response) { bad_request_response }

            should_respond_with 400, 'and does not create the client' do
              # Make sure the resource has not been persisted
              persisted_resource_response.should look_like not_found_response

              # Make sure we cannot use this credential
              get(resource_url, created_requestor).should look_like unauthorized_response
            end
          end
        end

        def rejects_public_key_on_update_with(adjective, _options = {})
          context "with a #{adjective} public key" do
            let(:requestor) { platform.admin_user }
            let(:public_key) { instance_eval_if_proc(_options[:public_key]) }
            let(:expected_response) { bad_request_response }
            should_respond_with 400, 'and does not update the key' do
              # Verify the client has not changed
              persisted_resource_response.should look_like updated_response
              # Verify the key has not changed
              get("#{resource_url}/keys/default", requestor).should look_like ({ status: 200,
                                                                                   body: { "public_key" => test_client_public_key } })
              # Verify that we can use the original credentials
              get(resource_url, test_client_requestor).should look_like updated_response
            end
          end
        end
      end # ClassMethods

    end # OpenSourceClientUtil
  end # RSpec
end # Pedant
