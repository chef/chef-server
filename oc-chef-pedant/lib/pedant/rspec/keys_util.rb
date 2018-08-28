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

require 'rspec/core/shared_context'

module Pedant
  module RSpec
    module KeysUtil
      extend ::RSpec::Core::SharedContext
      extend Pedant::Concern

      def org_base_url
        "#{platform.server}/organizations/#{org_name}"
      end

      def add_user_key(user, key, key_name, options = {})
        payload = add_key_body(key, key_name, options)
        requestor = options[:requestor].nil? ? superuser : options[:requestor]
        post("#{platform.server}/users/#{user}/keys", requestor, payload: payload)
      end

      def add_client_key(org, client, key, key_name, options = {})
        payload = add_key_body(key, key_name, options)
        requestor = options[:requestor].nil? ? superuser : options[:requestor]
        post("#{platform.server}/organizations/#{org}/clients/#{client}/keys", requestor, payload: payload)
      end

      def list_user_keys(user, requestor)
        get("#{platform.server}/users/#{user}/keys", requestor)
      end

      def list_org_scoped_user_keys(org, user, requestor)
        get("#{platform.server}/organizations/#{org}/users/#{user}/keys", requestor)
      end

      def get_user_key(user, requestor, key)
        get("#{platform.server}/users/#{user}/keys/#{key}", requestor)
      end

      def get_org_scoped_user_key(org, user, requestor, key)
        get("#{platform.server}/organizations/#{org}/users/#{user}/keys/#{key}", requestor)
      end

      def list_client_keys(org, client, requestor)
        get("#{platform.server}/organizations/#{org}/clients/#{client}/keys", requestor)
      end

      def get_client_key(org, client, requestor, key)
        get("#{platform.server}/organizations/#{org}/clients/#{client}/keys/#{key}", requestor)
      end

      def delete_user_key(user, key_name, options = {})
        requestor = options[:requestor] || superuser
        delete("#{platform.server}/users/#{user}/keys/#{key_name}", requestor)
      end

      def delete_client_key(org, client, key_name, options = {})
        requestor = options[:requestor] ? options[:requestor] : superuser
        delete("#{platform.server}/organizations/#{org}/clients/#{client}/keys/#{key_name}", requestor)
      end

      def add_key_body(key, key_name, options)
        request = {
          "name" => key_name,
          "expiration_date" => options[:expires] || "infinity"
        }

        if key == :create_key
          request["create_key"] = true
        else
          request["public_key"] = keys[key][:public]
        end

        request
      end

      # These behaviors are identical for POSTing to create a client or user key
      shared_context "basic keys POST validation" do
        # Generate validation tests
        { "when name is empty"   => { replace: { "name" => "" } },
          "when name is invalid" => { replace: { "name" => "key the first" } },
          "when name is missing" => { delete:  [ "name" ] },
          "when date is invalid" => { replace: { "expiration_date" => "2010-09-32T10:00:00Z" } },
          "when date is empty"   => { replace: { "expiration_date" => "" } },
          "when date is missing" => { delete:  [ "expiration_date" ] },
          "when public key is not a valid key" => { replace: { "public_key" => "Nope." } },
          "when public key is missing" => { delete: [ "public_key" ] },
          "when both a public_key and create_key are present" => { replace: { "create_key" => true } }
        }.each do |desc, setup|
          it "#{desc} it responds with 400", :validation do
            setup = {:replace=>{}, :delete => []}.merge(setup)
            payload = key_payload.dup
            payload.merge!(setup[:replace])
            setup[:delete].each { |field| payload.delete(field) }

            post(key_url, superuser, payload: payload).should look_like(status: 400)
          end
        end

        context "when a key of the same name already exists" do
          let(:payload) { key_payload.merge("name" => "default") }

          it "responds with 409" do
            post(key_url, superuser, payload: payload)
              .should look_like(code: 409)
          end
        end

        context "when all fields are present and valid" do
          after do
            delete("#{key_url}/#{key_payload['name']}", superuser).should look_like(status: 200)
          end

          it "should create a key with proper response and Location header" do
            expected_location = "#{key_url}/#{key_payload['name']}"
            response = post("#{key_url}", superuser, payload: key_payload)
            response.should look_like(
              {
                :status => 201,
                :body_exact => { "uri" => expected_location },
                :headers => [ "Location" => expected_location ]
              })
          end

          it "and infinity date is specfied it should still create a key with proper response and Location header" do
            expected_location = "#{key_url}/#{key_payload['name']}"
            key_payload["expiration_date"] = "infinity"
            response = post("#{key_url}", superuser, payload: key_payload)
            response.should look_like(
              {
                :status => 201,
                :body_exact => { "uri" => expected_location },
                :headers => [ "Location" => expected_location ]
              })
          end

          it "when 'create_key' is false alongside a public key it should not generate a private key" do
            key_payload["create_key"] = false
            expected_location = "#{key_url}/#{key_payload['name']}"
            response = post(key_url, superuser, payload: key_payload)
            response.should look_like(status: 201,
                                      body_exact: { "uri" => expected_location })
          end

          it "when 'create_key' : true is specified in lieu of public key it should generate a new private key and reply with it in the body" do
            key_payload["create_key"] = true
            key_payload.delete("public_key")
            expected_location = "#{key_url}/#{key_payload['name']}"
            response = post(key_url, superuser, payload: key_payload)
            response.should look_like(status: 201,
                                      body_exact: { "uri" => expected_location,
                                                    "private_key" => /.*BEGIN (RSA )?PRIVATE.*/  })
          end
        end
      end

      shared_context "basic keys PUT validation" do
        before (:each) do
          @named_key_url = "#{key_url}/#{key_payload["name"]}"
          post(key_url, superuser, payload: key_payload).should look_like(status: 201)
        end
        after (:each) do
          # TODO we really only need to delte/recreate if we succeed in a PUT?
          delete(@named_key_url, superuser).should look_like(status: 200)
        end

        it "when all fields are modified it should update the key and send us back the updated body and header"  do
          key_payload['name'] = 'altname1'
          key_payload['public_key'] = keys[:key][:public]
          key_payload['expiration_date'] = unexpired_date_2
          expected_location = "#{key_url}/altname1"
          response = put(@named_key_url, superuser, payload: key_payload)
          @named_key_url = expected_location # Update so we can delete
          response.should look_like(status: 201, body_exact: key_payload,
                                    headers: ["Location" => expected_location])
        end
        it "when 'create_key' is false alongside a public key it should not generate a private key" do
          key_payload['create_key'] = false
          key_payload['public_key'] = keys[:key][:public]
          key_payload.delete("name")
          key_payload.delete("expiration_date")
          response = put(@named_key_url, superuser, payload: key_payload)
          response.should look_like(status: 200,
                                    body_exact: { "public_key" => key_payload['public_key'] })
        end
        it "when 'create_key' : true is specified in lieu of public key it should generate a new private key and reply with it and the public_key in the body" do
            response = put(@named_key_url, superuser, payload: { "create_key" => true })
            response.should look_like(status: 200,
                                      body_exact: { "public_key" => /.*BEGIN (RSA )?PUBLIC KEY.*/,
                                                    "private_key" => /.*BEGIN (RSA )?PRIVATE KEY.*/ } )
        end
        it "when PUT body is empty it should fail with a 400" do
          put(@named_key_url, superuser, payload: {}).should look_like(status: 400)
        end

        # Note that ommitted fields are preserved, so we're not testing those as invalid updates.
        { "when name is empty" => {:replace => {"name" => ""}, :code => 400 },
          "when name is invalid" => {:replace => {"name" => "key the first"}, :code => 400 },
          "when date is empty" => {:replace => {"expiration_date" => ""}, :code => 400},
          "when date is invalid" => {:replace => {"expiration_date" => "2010-09-32T10:00:00"}, :code => 400},
          "when public key is empty" => {:replace => { "public_key" => ""}, :code => 400},
          "when public key is invalid" => {:replace => { "public_key" => "Nope."}, :code => 400},
          "when a key of the same name already exists" => {:replace => {"name" => "default"}, :code => 409},
          "when both a public_key and create_key are present" => {:replace => { "create_key" => true }, :code => 400},
          "when both a public_key and create_key are present but create_key is false" => {:replace => { "create_key" => false }, :code => 200}
        }.each do |desc, setup|
          it "#{desc} it responds with #{setup[:code]}" do
            setup = {:replace=>{}}.merge(setup)
            payload = key_payload.merge(setup[:replace])
            put(@named_key_url, superuser, payload:payload).should look_like(status: setup[:code])
          end
        end
      end

      # PATCHy PUT behaviors allow us to do partial updates via PUT. If a field
      # is not specified,  it should not be changed.
      shared_context "PUT like a PATCH" do
        before (:each) do
          @named_key_url = "#{key_url}/#{key_payload['name']}"
          post(key_url, superuser, payload: key_payload).should look_like(status: 201)
        end

        after (:each) do
          delete(@named_key_url, superuser).should look_like(status: 200)
        end

        it "when only name is present and is changed, responds with 201 and a Location header" do
          payload = { "name" =>  "altname1"}
          expected_location = "#{key_url}/altname1"
          put(@named_key_url, superuser, payload: payload ).should look_like(
            { :status => 201, :body_exact => payload,
              :headers => [ "Location" => expected_location ] })
          @named_key_url = expected_location
        end

        it "when only name is present and has not been changed" do
          payload = { "name" => key_payload["name"] }
          put(@named_key_url, superuser, payload: payload).should look_like(
            { :status => 200, :body_exact => payload,
              :no_headers => ["Location"] })
        end

        it "should only update  public_key when that is the only provided field" do
          payload = { "public_key" => keys[:key][:public] }
          put(@named_key_url, superuser, payload: payload).should look_like(
            { :status => 200, :body_exact => payload,:no_headers => ["Location"] })
        end

        it "should only update expiration date when that is the only provided field" do
          payload = { "expiration_date" =>  unexpired_date_2 }
          put(@named_key_url, superuser, payload: payload).should look_like(
            { :status => 200, :body_exact => payload,:no_headers => ["Location"] })
        end
      end
    end
  end
end
