# Copyright: Copyright 2015-2018 Chef Software, Inc.
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

require 'json'
require 'pedant/rspec/keys_util'
require_relative '../../shared_context/keys_context.rb'

describe "Client keys endpoint", :keys, :client_keys do
  include Pedant::RSpec::KeysUtil

  # Noise reducer.
  def requestor(who, key)
    Pedant::Requestor.new(who, key, preexisting: false)
  end

  # Generate a random 7-digit number
  def rand_id
    rand(10**7...10**8).to_s
  end

  shared(:keys) { @keys }

  let(:user) do
    { "name" => "pedant-keys-user-#{rand_id}",
      "public_key" => keys[:original_user][:public],
      "private_key" => keys[:original_user][:private]
    }
  end

  let(:user_payload) do
    {
      "username" => user['name'],
      "first_name" => user['name'],
      "middle_name" => user['name'],
      "last_name" => user['name'],
      "display_name" => user['name'],
      "email" => "#{user['name']}@#{user['name']}.com",
      "password" => "user-password",
      "public_key" => user['public_key']
    }
  end

  # In the spirit of eventual parallelization,
  # we'll be using our own org.
  shared(:org_name) do
    if Pedant.config[:org][:create_me]
      "pedant-keys-org-#{Time.now.to_i}"
    else
      Pedant.config[:org][:name]
    end
  end

  let(:client) do
    { "name" => "pedant-keys-client-#{rand_id}",
      "public_key" => keys[:original_client][:public],
      "private_key" => keys[:original_client][:private]
    }
  end

  let(:key_name) do
    "key-#{rand_id}"
  end

  let(:client_payload) do
    { "name" => client['name'], "public_key" => client['public_key'],
      "admin" => "true"
    }
  end

  let(:new_client_list_keys_response) do
    [ { "name" => "default",
        "uri" => "#{org_base_url}/clients/#{client['name']}/keys/default",
        "expired" => false
      }
    ]
  end

  shared(:unexpired_date_2) do
    "2049-12-24T21:00:00Z"
  end

  shared(:unexpired_date) do
    "2050-12-24T21:00:00Z"
  end

  shared(:expired_date) do
    "2012-01-01T00:00:00Z"
  end

  before(:all) do
    @keys = {}

    begin
      [ :original_client, :original_user,
        :key, :alt_key,
        :org_admin, :org_user, :org_client,
        :other_org_client, :other_org_user
      ].each do |name|
        @keys[name] = platform.gen_rsa_key(name)
      end
    rescue Exception => e
      puts "Error creating keys: #{e.message}"
      raise
    end

    # orgs static in the tests, only create once
    if Pedant.config[:org][:create_me]
      @test_org = platform.create_org(org_name)
    else
      @test_org = platform.org_from_config
    end
  end

  after(:all) do
    if Pedant.config[:org][:create_me] && Pedant.config[:delete_org]
      platform.delete_org(org_name)
    end
  end

  # create client before each test
  before(:each) do
    unless Pedant::Config[:tags].include?("~multiuser")
      post("#{platform.server}/users", superuser, payload: user_payload)
    end

    post("#{org_base_url}/clients", superuser, payload: client_payload)
  end

  # delete client after each test
  after(:each) do
    unless Pedant::Config[:tags].include?("~multiuser")
      delete("#{platform.server}/users/#{user['name']}", superuser)
    end

    delete("#{org_base_url}/clients/#{client['name']}", superuser)
  end

  context "when a new client is created via POST /organizations/:org/clients" do
    it "should insert a new default keys entry that is retrievable via the keys API" do
      list_client_keys(org_name, client['name'], superuser).should look_like(status: 200, body: new_client_list_keys_response)
    end
  end

  context "when a single key exists for a client" do
    context "when the key is uploaded via POST /clients" do
      it "should authenticate against the single key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], client['private_key'])).should look_like(status: 200)
      end
    end

    context "when the default key has been changed via the keys API", :authentication do
      before(:each) do
        delete_client_key(org_name, client['name'], "default")
        add_client_key(org_name, client['name'], :key, "default").should look_like(status: 201)
      end

      it "should authenticate against the updated key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like(status: 200)
      end

      it "should break for original default key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], client['private_key'])).should look_like(status: 401)
      end
    end
  end

  context "when a key has been generated for a client" do
    before(:each) do
        r = add_client_key(org_name, client['name'], :create_key, "genkey")
        r.should look_like(status: 201)
        @auth_privkey = parse(r)["private_key"]
    end

    after(:each) do
      delete_client_key(org_name, client['name'], "genkey")
    end

    it "should be able to authenticate with the generated key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], @auth_privkey)).should look_like(status: 200)
    end
  end

  context "when a key is deleted for a client" do
    before(:each) do
      add_client_key(org_name, client['name'], :alt_key, key_name).should look_like(status: 201)
    end

    it "should no longer be returned by the keys API" do
      delete_client_key(org_name, client['name'], key_name)
      list_client_keys(org_name, client['name'], superuser).should_not include(key_name)
    end

    it "should still contain other keys not yet deleted" do
      delete_client_key(org_name, client['name'], key_name)
      list_client_keys(org_name, client['name'], superuser).should include("default")
    end
  end

  context "when multiple keys exist for a client" do
    before(:each) do
      add_client_key(org_name, client['name'], :alt_key, "alt-#{key_name}").should look_like(status: 201)
      add_client_key(org_name, client['name'], :key, key_name).should look_like(status: 201)
    end

    context "should properly authenticate against either keys" do
      it "should properly authenticate against the first key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like(status: 200)
      end

      it "should properly authenticate against the second key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:alt_key][:private])).should look_like(status: 200)
      end
    end
  end

  context "when a client's default key has an expiration date" do
    before(:each) do
      delete_client_key(org_name, client['name'], "default")
      add_client_key(org_name, client['name'], :key, "default", expires: "2025-03-24T21:00:00Z").should look_like(status: 201)
    end

    context "and is updated via a PUT to /organizations/:org/clients/:client" do
      before(:each) do
        original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
        original_data['public_key'] = keys[:alt_key][:public]
        put("#{org_base_url}/clients/#{client['name']}", superuser, payload: original_data)
      end

      it "should no longer have an expiration date when queried via the keys API" do
        get_client_key(org_name, client['name'], superuser, "default")
          .should look_like(status: 200, body: { "expiration_date" => "infinity" })
      end
    end
  end

  context "when a client key has an expiration date and isn't expired" do
    before(:each) do
      add_client_key(org_name, client['name'], :key, key_name, expires: "2025-03-24T21:00:00Z").should look_like(status: 201)
    end

    it "should authenticate against the key" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like(status: 200)
    end
  end

  context "when a key is expired for a client", :authentication do
    before(:each) do
      add_client_key(org_name, client['name'], :key, key_name, expires: "2012-12-24T21:00:00Z" ).should look_like(status: 201)
    end

    it "should fail against the expired key" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like(status: 401)
    end

    it "should succeed against other keys" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], client['private_key'])).should look_like(status: 200)
    end
  end

  context "when the default key for a client exists" do
    it "should return public_key field returned by GET /organization/:org/clients/:client and from the keys table should be the same" do
      client_api_public_key = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key']
      get_client_key(org_name, client['name'], superuser, "default")
        .should look_like(status: 200, body: { "public_key" => client_api_public_key })
    end
  end

  context "when a clients's default key is deleted via the keys API" do
    before(:each) do
      delete_client_key(org_name, client['name'], "default")
    end

    it "public field returned by /organizations/:org/clients/:client should be null" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should == nil
    end

    it "the keys API should not return a key named default" do
      list_client_keys(org_name, client['name'], superuser).should_not include("default")
    end
  end

  context "when a client is updated via PUT but the public_key is omitted" do
    before(:each) do
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data.delete("public_key")
      put("#{org_base_url}/clients/#{client['name']}", superuser, payload: original_data)
    end

    it "not modify the public key returned via GET /organizations/:org/clients/:client" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should == client['public_key']
    end

    it "should not clear the default key returned via the keys API" do
      get_client_key(org_name, client['name'], superuser, "default").should look_like(status: 200)
    end
  end

  context "when a client's default key has already been deleted via the keys API and then re-added via PUT to /organizations/:org/clients/:client" do
    before(:each) do
      delete_client_key(org_name, client['name'], "default")
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data['public_key'] = keys[:key][:public]
      put("#{org_base_url}/clients/#{client['name']}", superuser, payload: original_data)
    end

    it "should be shown in the clients's record via GET of the named client" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should include(keys[:key][:public])
    end

    it "should be present in the keys list" do
      list_client_keys(org_name, client['name'], superuser).should include("default")
    end

    it "should be able to authenticate with the updated default key" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like(status: 200)
    end
  end

  context "when the default key is updated for a client via a PUT to /organizations/:org/clients/:client" do
    before(:each) do
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data['public_key'] = keys[:key][:public]
      put("#{org_base_url}/clients/#{client['name']}", superuser, payload: original_data)
    end

    context "when the default key exists" do
      it "should update the default key in the keys table" do
        get_client_key(org_name, client['name'], superuser, "default")
          .should look_like(status: 200, body: { "public_key" => keys[:key][:public] })
      end

      it "should no longer contain the old default key" do
        response = get_client_key(org_name, client['name'], superuser, "default")
        response.should look_like(status: 200 )

        JSON.parse(response)["public_key"].should_not include client['public_key']
      end

      it "should return the new key from the /clients endpoint" do
        JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should include(keys[:key][:public])
      end
    end
  end

  context "when a client is PUT with public_key:null to /organizations/:org/clients/:client" do
    before(:each) do
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data['public_key'] = nil
      put("#{org_base_url}/clients/#{client['name']}", superuser, payload: original_data)
    end

    it "the key should remain unchanged via GET /organizations/:org/clients/:client" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should include(client['public_key'])
    end

    it "should leave the default key from the keys API list unmodified for that client" do
      list_client_keys(org_name, client['name'], superuser).should include("default")
    end
  end

  context "managing keys" do
    shared(:name_suffix) { "#{rand_id}" }
    shared(:org_admin_name) {"pedant-keys-admin-#{name_suffix}" }
    shared(:org_admin_user) {requestor(org_admin_name, keys[:org_admin][:private]) }
    shared(:org_user_name) {"pedant-keys-user-#{name_suffix}" }
    shared(:org_user) {requestor(org_user_name, keys[:org_user][:private]) }
    shared(:org_client_name) {"pedant-keys-client-#{name_suffix}" }
    shared(:org_client) {requestor(org_client_name, keys[:org_client][:private]) }

    shared(:other_org_name) { "pedant-keys-org-2-#{rand_id}" }
    shared(:other_org_user_name) { "#{other_org_name}-user" }
    shared(:other_org_user) {requestor(other_org_user_name, keys[:other_org_user][:private]) }
    shared(:other_org_client_name) { "#{other_org_name}-client" }
    shared(:other_org_client) {requestor(other_org_client_name, keys[:other_org_client][:private]) }

    shared(:other_org_user_payload) do
      { "public_key" => keys[:other_org_user][:public] }
    end

    shared(:org_client_payload) do
      { "name" => org_client_name,
        "public_key" => keys[:org_client][:public] }
    end

    shared(:org_admin_payload) do
      { "public_key" => keys[:org_admin][:public] }
    end

    shared(:org_user_payload) do
      { "public_key" => keys[:org_user][:public] }
    end

    let (:key_payload) do
      { "public_key" => keys[:alt_key][:public],
        "name" => "alt_key",
        "expiration_date" => "2100-12-31T23:59:59Z" }
    end

    let (:other_org_client_payload) do
      { "public_key" => keys[:other_org_client][:public],
        "name" => other_org_client_name }
    end

    before :all do
      unless Pedant::Config[:tags].include?("~multiuser")
        # This user is used in more tests than not, so create it for the full context.
        # Leave it up to the tests to determine if they need it associated
        platform.create_min_user(org_user_name, overrides: org_user_payload).should look_like(status: 201 )
        platform.create_min_user(org_admin_name, overrides: org_admin_payload).should look_like(status: 201 )
        platform.associate_user_with_org(org_name, org_admin_user)
        platform.add_user_to_group(org_name, org_admin_user, "admins")
        platform.create_org(other_org_name)
        platform.create_min_user(other_org_user_name, overrides: other_org_user_payload).should look_like(status: 201)
        platform.associate_user_with_org(other_org_name, other_org_user)
      end
    end

    after :all do
      unless Pedant::Config[:tags].include?("~multiuser")
        platform.remove_user_from_group(org_name, org_admin_user, "admins", superuser)
        platform.delete_user(org_admin_user)
        platform.delete_user(org_user)
        platform.delete_user(other_org_user)
        platform.delete_org(other_org_name)
      end
    end

    context "posting keys" do
      let (:key_url) { "#{org_base_url}/clients/#{org_client_name}/keys" }

      before(:each) do
        post("#{org_base_url}/clients", superuser, payload: org_client_payload).should look_like(status: 201)
      end

      after(:each) do
        delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like(status: 200)
      end

      it_behaves_like "basic keys POST validation"

      it "for a client that doesn't exist it should respond with 404" do
        post("#{org_base_url}/clients/bob/keys", superuser, payload: key_payload).should look_like(status: 404)
      end

      context "POST /organizations/:org/clients/:client/keys as...", :authorization do
        it "an invalid user fails with 401", :authentication do
          post("#{org_base_url}/clients/#{org_client_name}/keys", requestor("bob", user['private_key']), payload: key_payload).should look_like(status: 401)
        end

        it "the org client itself succeeds" do
          post("#{org_base_url}/clients/#{org_client_name}/keys", org_client, payload: key_payload).should look_like(status: 201)
        end

        it "the superuser succeeds" do
          post("#{org_base_url}/clients/#{org_client_name}/keys", superuser, payload: key_payload).should look_like(status: 201)
        end

        context "another user in the org", :authorization do
          it "fails with 403", :authorization do
            post("#{org_base_url}/clients/#{org_client_name}/keys", org_user, payload: key_payload).should look_like(status: 403)
          end
        end

        context "as an org admin of a member org" do
          it "succeeds" do
            post("#{org_base_url}/clients/#{org_client_name}/keys", org_admin_user, payload: key_payload).should look_like(status: 201)
          end
        end
      end
    end

    context "PUT", :authorization do
      let (:key_url) { "#{org_base_url}/clients/#{org_client_name}/keys" }

      before(:all) do
        post("#{org_base_url}/clients", superuser, payload: org_client_payload).should look_like(status: 201)
      end

      after(:all) do
        delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like(status: 200)
      end

      it_behaves_like "PUT like a PATCH"
      it_behaves_like "basic keys PUT validation"
    end

    context "DELETE", :authorization do
      context "/organizations/:org/clients/:client/keys/:key" do
        before(:all) do
          post("#{org_base_url}/clients", superuser, payload: org_client_payload).should look_like(status: 201)
        end

        after(:all) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like(status: 200)
        end

        before (:each) do
          add_client_key(org_name, org_client_name, :alt_key, "alt_key").should look_like(status: 201)
        end

        after (:each) do
          delete_client_key(org_name, org_client_name, "alt_key")
        end

        it "fails with 404" do
          delete("#{org_base_url}/clients/bobclient/keys/default", superuser).should look_like(status: 404)
        end

        it "and the key does not exist it fails with 404" do
          delete("#{org_base_url}/clients/#{org_client_name}/keys/badkey", superuser).should look_like(status: 404)
        end

        context "as..." do # DELETE $endpoint as
          it "the client itself, authenticating with a different key should succeed" do
            delete_client_key(org_name, org_client_name, "alt_key", requestor: org_client).should look_like(status: 200)
          end

          it "the client itself, authenticating with the key it is trying to delete should fail with 403", :authorization do
            r = delete_client_key(org_name, org_client_name, "alt_key", requestor: requestor(org_client_name, keys[:alt_key][:private]))
            r.should look_like(status: 403, body_exact: { "error" => "The key 'alt_key' was used to authenticate this request and cannot be modified or deleted." })
          end

          context "a client in the same org" do
            before do
              post("#{org_base_url}/clients", superuser, payload: other_org_client_payload ).should look_like(status: 201)
            end

            after do
              delete("#{org_base_url}/clients/#{other_org_client_name}", superuser).should look_like(status: 200)
            end

            it "should fail with a 403", :authorization do
              delete_client_key(org_name, org_client_name, "alt_key", requestor: other_org_client).should look_like(status: 403)
            end
          end

          context "a client in a different org", :authentication do
            before do
              post("#{platform.server}/organizations/#{other_org_name}/clients", superuser, payload: other_org_client_payload).should look_like(status: 201)
            end

            after do
              delete("#{platform.server}/organizations/#{other_org_name}/clients/#{other_org_client_name}", superuser).should look_like(status: 200)
            end

            it "should fail with a 401" do
              delete_client_key(org_name, org_client_name, "alt_key", requestor: other_org_client).should look_like(status: 401)
            end
          end

          context "a user in the same org" do
            before do
              platform.associate_user_with_org(org_name, org_user).should look_like(status: 201)
            end

            after do
              platform.remove_user_from_org(org_name, org_user)
            end

            it "should fail with a 403", :authorization do
              delete_client_key(org_name, org_client_name, "alt_key", requestor: org_user).should look_like(status: 403)
            end

          end

          it "a user not affiliated with the org should fail with a 403", :authorization do
            delete_client_key(org_name, org_client_name, "alt_key", requestor: other_org_user).should look_like(status: 403)
          end

          it "the org admin should succeed" do
            delete_client_key(org_name, org_client_name, "alt_key", requestor: org_admin_user).should look_like(status: 200)
          end
        end
      end
    end

    context "PUT", :authorization do
      context "/organizations/:org/clients/:client/keys/:key" do
        before(:all) do
          post("#{org_base_url}/clients", superuser, payload: org_client_payload).should look_like(status: 201)
          add_client_key(org_name, org_client_name, :alt_key, "alt_key").should look_like(status: 201)
          @key_url = "#{org_base_url}/clients/#{org_client_name}/keys/alt_key"
        end

        after(:all) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like(status: 200)
          delete_client_key(org_name, org_client_name, "alt_key")
        end

        it "to a key for an invalid actor fails with 404" do
          put("#{org_base_url}/clients/bobclient/keys/default", superuser, payload: key_payload).should look_like(status: 404)
        end

        it "to a key for a valid actor, but the key does not exist" do
          put("#{org_base_url}/clients/#{org_client_name}/keys/badkey", superuser, payload: key_payload).should look_like(status: 404)
        end

        context "as..." do # PUT $endpoint as
          it "the client itself, authenticating with a different key should succeed" do
            put(@key_url, org_client, payload: key_payload).should look_like(status: 200)
          end

          it "the client itself, authenticating with the key it is trying update should fail with 403", :authorization do
            res = put(@key_url, requestor(org_client_name, keys[:alt_key][:private]), payload: key_payload)
            res.should look_like(
              status: 403,
              body_exact: { "error" => "The key 'alt_key' was used to authenticate this request and cannot be modified or deleted."}
            )
          end

          context "a client in the same org" do
            before do
              post("#{org_base_url}/clients", superuser, payload: other_org_client_payload ).should look_like(status: 201)
            end

            after do
              delete("#{org_base_url}/clients/#{other_org_client_name}", superuser).should look_like(status: 200)
            end

            it "should fail with a 403", :authorization do
              put(@key_url, other_org_client, payload: key_payload).should look_like(status: 403)
            end
          end

          context "a client in a different org", :authentication do
            before do
              post("#{platform.server}/organizations/#{other_org_name}/clients", superuser, payload: other_org_client_payload).should look_like(status: 201)
            end

            after do
              delete("#{platform.server}/organizations/#{other_org_name}/clients/#{other_org_client_name}", superuser).should look_like(status: 200)
            end

            it "should fail with a 401" do
              put(@key_url, other_org_client, payload: key_payload).should look_like(status: 401)
            end
          end

          context "a user in the same org" do
            before do
              platform.associate_user_with_org(org_name, org_user).should look_like(status: 201)
            end

            after do
              platform.remove_user_from_org(org_name, org_user)
            end

            it "should fail with a 403", :authorization do
              put(@key_url, org_user, payload: key_payload).should look_like(status: 403)
            end

          end

          it "a user not affiliated with the org should fail with a 403", :authorization do
            put(@key_url, other_org_user, payload: key_payload).should look_like(status: 403)
          end

          it "the org admin should succeed" do
            put(@key_url, org_admin_user, payload: key_payload).should look_like(status: 200)
          end
        end
      end
    end

    context "listing key(s)" do
      context "when multiple keys are present" do
        before(:each) do
          post("#{org_base_url}/clients", superuser, payload: org_client_payload).should look_like(status: 201)
          add_client_key(org_name, org_client_name, :key, "key1", :expires =>  unexpired_date).should look_like({:status=>201})
          add_client_key(org_name, org_client_name, :alt_key, "key2", :expires => expired_date).should look_like({:status=>201})
        end

        after(:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like(status: 200)
        end

        shared_examples_for 'successful client key get' do
          context "when GET /organizations/org/clients/client/keys is called (list keys)" do
            it "all keys should be listed with correct expiry indicators" do
              list_client_keys(org_name, org_client_name, current_requestor).should look_like({
                status: 200,
                body_exact: [
                  { "name" => "default", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/default", "expired" => false },
                  { "name" => "key1", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/key1", "expired" => false },
                  { "name" => "key2", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/key2", "expired" => true}
                ]})
            end

            context "when GET is called on the URIs that are returned" do
              it "should return status 200" do
                client_keys = list_client_keys(org_name, org_client_name, current_requestor)
                client_keys.should look_like(status: 200)
                JSON.parse(client_keys).each do |key|
                  get(key["uri"], current_requestor).should look_like(status: 200)
                end
              end
            end
          end

          context "when GET /organizations/org/clients/client/keys/key is called (get single key)" do
            context "when it is called for each valid key" do
              it "should properly return the default key with valid expiration" do
                get_client_key(org_name, org_client_name, current_requestor, "default").should look_like({
                  :status => 200,
                  :body => { "name" => "default", "public_key" => keys[:org_client][:public], "expiration_date" => "infinity" }
                  })
              end

              it "should properly return the first custom key with valid expiration" do
                get_client_key(org_name, org_client_name, current_requestor, "key1").should look_like({
                  :status => 200,
                  :body => { "name" => "key1", "public_key" => keys[:key][:public], "expiration_date" =>  unexpired_date}
                  })
              end

              it "should properly return the second custom key with valid expiration" do
                get_client_key(org_name, org_client_name, current_requestor, "key2").should look_like({
                  :status => 200,
                  :body => { "name" => "key2", "public_key" => keys[:alt_key][:public], "expiration_date" => expired_date }
                  })
              end
            end
          end
        end # shared_examples_for successful client key get

        # contexts from '../../shared_context/keys_context.rb'
        context 'when the superuser is making the request' do
          include_context 'when the current_requestor is the superuser'
          it_should_behave_like 'successful client key get'
        end

        context 'when a user that is a member of the same org is making the request', :multiuser do
          include_context 'when the current_requestor is a user in the main org'
          it_should_behave_like 'successful client key get'
        end

        context 'when a client is a member of the same org is making the request' do
          include_context 'when the current_requestor is a client in the main org'
          it_should_behave_like 'successful client key get'
        end

        context 'when a user is not a member of the same org is making a request', :multiuser do
          include_context 'when the current_requestor is a user in a different org'

          it 'list client keys returns a 403', :authentication do
            list_client_keys(org_name, org_client_name, current_requestor).should look_like(:status => 403)
          end

          it 'get client keys returns a 403', :authentication do
            get_client_key(org_name, org_client_name, current_requestor, 'key1').should look_like(:status => 403)
          end
        end

        context 'when a client is not a member of the same org is making a request' do
          include_context 'when the current_requestor is a client in a different org'

          it 'list client keys returns a 401', :authentication do
            list_client_keys(org_name, org_client_name, current_requestor).should look_like(:status => 401)
          end

          it 'get client keys returns a 401', :authentication do
            get_client_key(org_name, org_client_name, current_requestor, 'key1').should look_like(:status => 401)
          end
        end

        # public_key_read_access testing
        shared_examples_for 'multiple actors READ access to the client keys endpoints depends on public_key_read_access membership' do
          context 'when an actor is removed from the public_key_read_access group' do
            before do
              platform.remove_group_from_group(org_name, groupname, 'public_key_read_access')
            end

            after do
              platform.add_group_to_group(org_name, groupname, 'public_key_read_access')
            end

            it 'the first actor can no longer list client keys, returning a 403', :authentication do
              list_client_keys(org_name, test_client_name_1, current_requestor).should look_like(:status => 403)
              list_client_keys(org_name, test_client_name_2, current_requestor).should look_like(:status => 403)
            end

            it 'the first actor can no longer get client keys, returning a 403', :authentication do
              get_client_key(org_name, test_client_name_1, current_requestor, 'default').should look_like(:status => 403)
              get_client_key(org_name, test_client_name_2, current_requestor, 'default').should look_like(:status => 403)
            end

            it 'the second actor can no longer list client keys, returning a 403', :authentication do
              list_client_keys(org_name, test_client_name_1, other_requestor).should look_like(:status => 403)
              list_client_keys(org_name, test_client_name_2, other_requestor).should look_like(:status => 403)
            end

            it 'the second actor can no longer get client keys, returning a 403', :authentication do
              get_client_key(org_name, test_client_name_1, other_requestor, 'default').should look_like(:status => 403)
              get_client_key(org_name, test_client_name_2, other_requestor, 'default').should look_like(:status => 403)
            end

            context 'when a single actor is added back into the the public_key_read_access group' do
              before do
                platform.send(add_method, org_name, current_requestor, 'public_key_read_access')
              end

              after do
                platform.send(remove_method, org_name, current_requestor, 'public_key_read_access')
              end

              it 'other actors, by default, can no longer list client keys, returning a 403', :authentication do
                list_client_keys(org_name, test_client_name_1, other_requestor).should look_like(:status => 403)
                list_client_keys(org_name, test_client_name_2, other_requestor).should look_like(:status => 403)
              end

              it 'other actors, by default, can no longer get client keys, returning a 403', :authentication do
                get_client_key(org_name, test_client_name_1, other_requestor, 'default').should look_like(:status => 403)
                get_client_key(org_name, test_client_name_2, other_requestor, 'default').should look_like(:status => 403)
              end

              it 'the added actor can list client keys', :authentication do
                list_client_keys(org_name, test_client_name_1, current_requestor).should look_like(:status => 200)
                list_client_keys(org_name, test_client_name_2, current_requestor).should look_like(:status => 200)
              end

              it 'the added actor can get client keys', :authentication do
                get_client_key(org_name, test_client_name_1, current_requestor, 'default').should look_like(:status => 200)
                get_client_key(org_name, test_client_name_2, current_requestor, 'default').should look_like(:status => 200)
              end
            end
          end
        end #shared_examples_for

        context 'when multiple clients exist' do
          before(:all) do
            @client_name_1 = "pedant_test_client_#{rand_id}"
            @client_name_2 = "pedant_test_client_2_#{rand_id}"
            @client_1 = platform.create_client(@client_name_1, @test_org)
            @client_2 = platform.create_client(@client_name_2, @test_org)
          end

          after(:all) do
            platform.delete_client(@client_1, @test_org)
            platform.delete_client(@client_2, @test_org)
          end

          context 'when the first client is making requests with an unmodified public_key_read_access group' do
            let(:current_requestor) { @client_1 }
            it_should_behave_like 'successful client key get'
          end

          context 'when the second client is making requests with an unmodified public_key_read_access group' do
            let(:current_requestor) { @client_2 }
            it_should_behave_like 'successful client key get'
          end

          context 'when clients are added and removed from the public_key_read_access group' do
            let(:test_client_name_1) { @client_name_1 }
            let(:test_client_name_2) { @client_name_2 }
            let(:current_requestor) { @client_1 }
            let(:other_requestor) { @client_2 }
            let(:groupname) { 'clients' }
            let(:add_method) { :add_client_to_group }
            let(:noun) { :client }
            let(:remove_method) { :remove_client_from_group }

            it_should_behave_like 'multiple actors READ access to the client keys endpoints depends on public_key_read_access membership'
          end

          context 'when there are multiple users associated to the org', :multiuser do
            before(:all) do
              @user_1 = platform.create_user("pedant_test_user_#{rand_id}")
              @user_2 = platform.create_user("pedant_test_user_2_#{rand_id}")
              platform.associate_user_with_org(org_name, @user_1)
              platform.associate_user_with_org(org_name, @user_2)
            end

            after(:all) do
              platform.delete_user(@user_1)
              platform.delete_user(@user_2)
            end

            context 'when the user client is making requests with an unmodified public_key_read_access group' do
              let(:current_requestor) { @user_1 }
              it_should_behave_like 'successful client key get'
            end

            context 'when the user client is making requests with an unmodified public_key_read_access group' do
              let(:current_requestor) { @user_2 }
              it_should_behave_like 'successful client key get'
            end

            context 'when users are added and removed from the public_key_read_access group' do
              let(:test_client_name_1) { @client_name_1 }
              let(:test_client_name_2) { @client_name_2 }
              let(:current_requestor) { @user_1 }
              let(:other_requestor) { @user_2 }
              let(:groupname) { 'users' }
              let(:add_method) { :add_user_to_group }
              let(:noun) { :user }
              let(:remove_method) { :remove_user_from_group }

              it_should_behave_like 'multiple actors READ access to the client keys endpoints depends on public_key_read_access membership'
            end
          end
        end
      end # context when multiple keys are present
    end # context listing key(s)
  end # context managing keys
end # describe Client keys endpoints
