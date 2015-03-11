# Copyright: Copyright (c) 2015 Chef Software, Inc.
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

describe "/keys endpoint", :keys do

  # Noise reducer.
  def requestor(who, key)
    Pedant::Requestor.new(who, key, preexisting: false)
  end

  shared(:keys) {@keys}
  let(:user) do
    {
      "name" => "pedant-keys-user-#{Time.now.to_i}",
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
  $org_name = "pedant-keys-org-#{Time.now.to_i}"


  let(:client) do
    {
      "name" => "pedant-keys-client-#{Time.now.to_i}",
      "public_key" => keys[:original_client][:public],
      "private_key" => keys[:original_client][:private]
    }
  end

  let(:key_name) do
    "key-#{Time.now.to_i}"
  end

  let(:client_payload) do
    { "name" => client['name'], "public_key" => client['public_key'],
      "admin" => "true"
    }
  end

  let(:new_user_list_keys_response) do
    [
      { "name" => "default", "uri" => "#{platform.server}/users/#{user['name']}/keys/default", "expired" => false}
    ]
  end

  let(:new_client_list_keys_response) do
    [
      { "name" => "default", "uri" => "#{org_base_url}/clients/#{client['name']}/keys/default", "expired" => false }
    ]
  end

  let(:unexpired_date) do
    "2017-12-24T21:00:00Z"
  end

  let(:expired_date) do
    "2012-01-01T00:00:00Z"
  end

  def org_base_url()
    "#{platform.server}/organizations/#{$org_name}"
  end

  def delete_client_key(org, client, key_name, options = {})
    requestor = options[:requestor] ? options[:requestor] : superuser
    delete("#{platform.server}/organizations/#{org}/clients/#{client}/keys/#{key_name}",  requestor)
  end

  def delete_user_key(user, key_name, options = {})
    requestor = options[:requestor] ? options[:requestor] : superuser
    delete("#{platform.server}/users/#{user}/keys/#{key_name}",  requestor)
  end

  def add_client_key(org, client, key, key_name, options = {})
    request =  { "name" => key_name, "public_key" => keys[key][:public],
                 "expiration_date" => options[:expires] || "infinity" }
    requestor = options[:requestor].nil? ? superuser : options[:requestor]
    post("#{platform.server}/organizations/#{org}/clients/#{client}/keys",  requestor, :payload => request)
  end

  def add_user_key(user, key, key_name, options = {})
    request =  { "name" => key_name, "public_key" => keys[key][:public],
                 "expiration_date" => options[:expires] || "infinity"  }
    requestor = options[:requestor].nil? ? superuser : options[:requestor]
    post("#{platform.server}/users/#{user}/keys",  requestor, :payload => request)
  end

  def list_user_keys(user, requestor)
    get("#{platform.server}/users/#{user}/keys", requestor)
  end

  def get_user_key(user, requestor, key)
    get("#{platform.server}/users/#{user}/keys/#{key}", requestor)
  end

  def list_client_keys(org, client, requestor)
    get("#{platform.server}/organizations/#{org}/clients/#{client}/keys", requestor)
  end

  def get_client_key(org, client, requestor, key)
    get("#{platform.server}/organizations/#{org}/clients/#{client}/keys/#{key}", requestor)
  end

  before(:all) do
    @keys = {}
    begin
      [:original_client, :original_user, :key, :alt_key,
       :org_admin, :org_user, :org_client, :other_org_client,
       :other_org_user].map do |x|
        priv = Tempfile.new("pedant-key-#{x}")
        pub = Tempfile.new("pedant-key-#{x}.pub")
        `openssl genrsa -out #{priv.path} 2048 1>/dev/null 2>&1`
        `openssl rsa -in #{priv.path} -pubout -out #{pub.path} 2>/dev/null`
        @keys[x] = {
          :path => "#{pub.path}",
          :private => File.read(priv.path),
          :public => File.read(pub.path)
           }
        priv.close
        priv.unlink
        pub.close
        pub.unlink
        end
    rescue Exception => e
      puts "Error creating keys: #{e.message}"
      raise
    end

    # orgs static in the tests, only create once
    post("#{platform.server}/organizations", superuser, :payload => {  "name" => $org_name, "full_name" => $org_name } ).should look_like({:status => 201})
  end

  after(:all) do
    platform.delete_org($org_name)
  end

  # create user and client before each test
  before(:each) do
    post("#{platform.server}/users", superuser, :payload => user_payload)
    post("#{org_base_url}/clients", superuser, :payload => client_payload)
  end

  # delete user and client after each test
  after(:each) do
    delete("#{platform.server}/users/#{user['name']}", superuser)
    delete("#{org_base_url}/clients/#{client['name']}", superuser)
  end

  context "when a new user is created via POST /users" do
    it "should insert a new default keys entry that is retrievable via the keys API" do
      list_user_keys(user['name'], superuser).should look_like( { :status=> 200, :body => new_user_list_keys_response} )
    end
  end

  context "when a new client is created via POST /organizations/:org/clients" do
    it "should insert a new default keys entry that is retrievable via the keys API" do
      list_client_keys($org_name, client['name'], superuser).should look_like( { :status=> 200, :body => new_client_list_keys_response} )
    end
  end

  context "when a single key exists for a user" do
    context "when the key is uploaded via POST /users" do
      it "should authenticate against the single key" do
        get("#{platform.server}/users/#{user['name']}", requestor(user['name'], user['private_key'])).should look_like({:status => 200})
      end
    end

    context "when the default key has been changed via the keys API", :authentication do
      before(:each) do
        delete_user_key(user['name'], "default").should look_like({:status => 200})
        add_user_key(user['name'], :alt_key, "default").should look_like({:status => 201})
      end
      it "should authenticate against the updated key" do
        get("#{platform.server}/users/#{user['name']}", requestor(user['name'], keys[:alt_key][:private])).should look_like({:status => 200})
      end
      it "should break for original default key" do
        get("#{platform.server}/users/#{user['name']}", requestor(user['name'], user['private_key'])).should look_like({:status => 401})
      end
    end
  end

  context "when a single key exists for a client" do
    context "when the key is uploaded via POST /clients" do
      it "should authenticate against the single key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], client['private_key'])).should look_like({:status => 200})
      end
    end

    context "when the default key has been changed via the keys API", :authentication do
      before(:each) do
        delete_client_key($org_name, client['name'], "default")
        add_client_key($org_name, client['name'], :key, "default").should look_like({:status=>201})
      end
      it "should authenticate against the updated key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like({:status => 200})
      end
      it "should break for original default key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], client['private_key'])).should look_like({:status => 401})
      end
    end
  end

  context "when a key is deleted for a user" do
    before(:each) do
        add_user_key(user['name'], :alt_key, key_name).should look_like({:status => 201})
    end
    it "should not longer be returned by the keys API" do
      delete_user_key(user['name'], key_name)
      list_user_keys(user['name'], superuser).should_not include(key_name)
    end
    it "should still contain other keys not yet deleted" do
      delete_user_key(user['name'], key_name)
      list_user_keys(user['name'], superuser).should include("default")
    end
  end

  context "when a key is deleted for a client" do
    before(:each) do
      add_client_key($org_name, client['name'], :alt_key, key_name).should look_like({:status=>201})
    end
    it "should no longer be returned by the keys API" do
      delete_client_key($org_name, client['name'], key_name)
      list_client_keys($org_name, client['name'], superuser).should_not include(key_name)
    end
    it "should still contain other keys not yet deleted" do
      delete_client_key($org_name, client['name'], key_name)
      list_client_keys($org_name, client['name'], superuser).should include("default")
    end
  end

  context "when multiple keys exist for a user" do
    before(:each) do
      add_user_key(user['name'], :alt_key, "alt-#{key_name}").should look_like({:status => 201})
      add_user_key(user['name'], :key, key_name).should look_like({:status => 201})
    end
    context "should properly authenticate against either keys" do
      it "should properly authenticate against the second key" do
        get("#{platform.server}/users/#{user['name']}", requestor(user['name'], keys[:key][:private])).should look_like({:status => 200})
      end
      it "should properly authenticate against the first key" do
        get("#{platform.server}/users/#{user['name']}", requestor(user['name'], keys[:alt_key][:private])).should look_like({:status => 200})
      end
    end
  end

  context "when multiple keys exist for a client" do
    before(:each) do
        add_client_key($org_name, client['name'], :alt_key, "alt-#{key_name}").should look_like({:status=>201})
        add_client_key($org_name, client['name'], :key, key_name).should look_like({:status=>201})
    end
    context "should properly authenticate against either keys" do
      it "should properly authenticate against the first key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like({:status => 200})
      end
      it "should properly authenticate against the second key" do
        get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:alt_key][:private])).should look_like({:status => 200})
      end
    end
  end

  context "when a user key has an expiration date and isn't expired" do
    before(:each) do
      add_user_key(user['name'], :key, key_name, :expires => "2017-12-24T21:00:00Z").should look_like({:status => 201})
    end
    it "should authenticate against the key" do
      get("#{platform.server}/users/#{user['name']}", requestor(user['name'], keys[:key][:private])).should look_like({:status => 200})
    end
  end

  context "when a user's default key has an expiration date" do
    before(:each) do
      delete_user_key(user['name'], "default")
      add_user_key(user['name'], :key, "default", :expires => "2017-12-24T21:00:00Z").should look_like({:status => 201})
    end
    context "and is updated via a PUT to /users/:user" do
      before(:each) do
        original_data = JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))
        original_data['public_key'] = keys[:alt_key][:public]
        put("#{platform.server}/users/#{user['name']}", superuser, :payload => original_data)
      end
      it "will no longer have an expiration date when queried via the keys API" do
        get_user_key(user['name'], superuser, "default").should look_like({:status => 200, :body => { "expiration_date" => "infinity" }})
      end
    end
  end

  context "when a client's default key has an expiration date" do
    before(:each) do
      delete_client_key($org_name, client['name'], "default")
      add_client_key($org_name, client['name'], :key, "default", :expires => "2017-12-24T21:00:00Z").should look_like({:status=>201})
    end
    context "and is updated via a PUT to /organizations/:org/clients/:client" do
      before(:each) do
        original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
        original_data['public_key'] = keys[:alt_key][:public]
        put("#{org_base_url}/clients/#{client['name']}", superuser, :payload => original_data)
      end
      it "should no longer have an expiration date when queried via the keys API" do
        get_client_key($org_name, client['name'], superuser, "default").should look_like({:status => 200, :body => { "expiration_date" => "infinity" }})
      end
    end
  end

  context "when a client key has an expiration date and isn't expired" do
    before(:each) do
      add_client_key($org_name, client['name'], :key, key_name, :expires => "2017-12-24T21:00:00Z").should look_like({:status=>201})
    end
    it "should authenticate against the key" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like({:status => 200})
    end
  end

  context "when a key is expired for a user", :authentication do
    before(:each) do
      add_user_key(user['name'], :key, key_name, :expires => "2012-12-24T21:00:00Z").should look_like({:status => 201})
    end
    it "should fail against the expired key" do
      get("#{platform.server}/users/#{user['name']}", requestor(user['name'], keys[:key][:private])).should look_like({:status => 401})
    end
    it "should succeed against other keys" do
      get("#{platform.server}/users/#{user['name']}", requestor(user['name'], user['private_key'])).should look_like({:status => 200})
    end
  end

  context "when a key is expired for a client", :authentication do
    before(:each) do
      add_client_key($org_name, client['name'], :key, key_name, :expires => "2012-12-24T21:00:00Z" ).should look_like({:status=>201})
    end
    it "should fail against the expired key" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like({:status => 401})
    end
    it "should succeed against other keys" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], client['private_key'])).should look_like({:status => 200})
    end
  end

  context "when the default key for a user exists" do
    it "the public_key field returned by GET /users/:user and from the keys table should be the same" do
      user_api_public_key = JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))['public_key']
      get_user_key(user['name'], superuser, "default").should look_like({:status => 200, :body => { "public_key" => user_api_public_key }})
    end
  end

  context "when the default key for a client exists" do
    it "should return public_key field returned by GET /organization/:org/clients/:client and from the keys table should be the same" do
      client_api_public_key = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key']
      get_client_key($org_name, client['name'], superuser, "default").should look_like({:status => 200, :body => { "public_key" => client_api_public_key }})
    end
  end

  context "when a user's default key is updated via the keys API" do
    before(:each) do
      delete_user_key(user['name'], "default")
      add_user_key(user['name'], :key, "default").should look_like({:status => 201})
    end

    it "should return the proper, updated key via /users/:user" do
      JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))['public_key'].should include(keys[:key][:public])
    end
  end

  context "when a user's default key is deleted via the keys API" do
    before(:each) do
      delete_user_key(user['name'], "default")
    end

    it "public field returned by /users/:user should be null" do
      JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))['public_key'].should == nil
    end
    it "the keys API should not return a key named default" do
      list_user_keys(user['name'], superuser).should_not include("default")
    end
  end

  context "when a clients's default key is deleted via the keys API" do
    before(:each) do
      delete_client_key($org_name, client['name'], "default")
    end

    it "public field returned by /organizations/:org/clients/:client should be null" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should == nil
    end
    it "the keys API should not return a key named default" do
      list_client_keys($org_name, client['name'], superuser).should_not include("default")
    end
  end

  context "when a user is updated via PUT but the public_key is omitted" do
    before(:each) do
      original_data = JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))
      original_data.delete("public_key")
      put("#{platform.server}/users/#{user['name']}", superuser, :payload => original_data)
    end
    it "should not modify the public key returned via GET /users/:user" do
      JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))['public_key'].should == user['public_key']
    end
    it "should not clear the default key returned via the keys API" do
      get_user_key(user['name'], superuser, "default").should look_like({:status => 200})
    end
  end

  context "when a client is updated via PUT but the public_key is omitted" do
    before(:each) do
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data.delete("public_key")
      put("#{org_base_url}/clients/#{client['name']}", superuser, :payload => original_data)
    end
    it "not modify the public key returned via GET /organizations/:org/clients/:client" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should == client['public_key']
    end
    it "should not clear the default key returned via the keys API" do
      get_client_key($org_name, client['name'], superuser, "default").should look_like({:status => 200})
    end
  end

  context "when a user's default key has already been deleted via the keys API and then re-added via PUT to /users/:user" do
    before(:each) do
      delete_user_key(user['name'], "default")
      original_data = JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))
      original_data['public_key'] = keys[:key][:public]
      put("#{platform.server}/users/#{user['name']}", superuser, :payload => original_data)
    end
    it "the correct key should be shown in the user's record via GET /users/:user" do
      JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))['public_key'].should include(keys[:key][:public])
    end
    it "should be present in the keys list" do
      list_user_keys(user['name'], superuser).should include("default")
    end
    it "should be able to authenticate with the updated default key" do
      get("#{platform.server}/users/#{user['name']}", requestor(user['name'], keys[:key][:private])).should look_like({:status => 200})
    end
  end

  context "when a client's default key has already been deleted via the keys API and then re-added via PUT to /organizations/:org/clients/:client" do
    before(:each) do
      delete_client_key($org_name, client['name'], "default")
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data['public_key'] = keys[:key][:public]
      put("#{org_base_url}/clients/#{client['name']}", superuser, :payload => original_data)
    end
    it "should be shown in the clients's record via GET of the named client" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should include(keys[:key][:public])
    end
    it "should be present in the keys list" do
      list_client_keys($org_name, client['name'], superuser).should include("default")
    end
    it "should be able to authenticate with the updated default key" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like({:status => 200})
    end
  end

  context "when the default key is updated for a user via a PUT to /users/:user" do
    before(:each) do
      original_data = JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))
      original_data['public_key'] = keys[:key][:public]
      put("#{platform.server}/users/#{user['name']}", superuser, :payload => original_data)
    end
    context "when the default key exists" do
      it "should update the default key in the keys table" do
        get_user_key(user['name'], superuser, "default").should look_like({:status => 200, :body => { "public_key" => keys[:key][:public]}})
      end
      it "should no longer contain the old default key" do
        response = get_user_key(user['name'], superuser, "default")
        response.should look_like({:status => 200} )
        JSON.parse(response)["public_key"].should_not include user['public_key']
      end
      it "should return the new key from the /users endpoint" do
        JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))['public_key'].should include(keys[:key][:public])
      end
    end
  end

  context "when the default key is updated for a client via a PUT to /organizations/:org/clients/:client" do
    before(:each) do
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data['public_key'] = keys[:key][:public]
      put("#{org_base_url}/clients/#{client['name']}", superuser, :payload => original_data)
    end
    context "when the default key exists" do
      it "should update the default key in the keys table" do
        get_client_key($org_name, client['name'], superuser, "default").should look_like({:status => 200,
                                                                                          :body => { "public_key" => keys[:key][:public]}})
      end
      it "should no longer contain the old default key" do
        response = get_client_key($org_name, client['name'], superuser, "default")
        response.should look_like({:status => 200} )
        JSON.parse(response)["public_key"].should_not include client['public_key']
      end
      it "should return the new key from the /users endpoint" do
        JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should include(keys[:key][:public])
      end
    end
  end

  context "when a user is PUT with public_key:null via /users/:user" do
    before(:each) do
      original_data = JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))
      original_data['public_key'] = nil
      put("#{platform.server}/users/#{user['name']}", superuser, :payload => original_data)
    end

    it "the key should remain unchanged via GET /users/:user" do
      JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))['public_key'].should include(user['public_key'])
    end
    it "should leave the default key from the keys API list present for that user" do
      list_user_keys(user['name'], superuser).should include("default")
    end
  end

  context "when a client is PUT with public_key:null to /organizations/:org/clients/:client" do
    before(:each) do
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data['public_key'] = nil
      put("#{org_base_url}/clients/#{client['name']}", superuser, :payload => original_data)
    end

    it "the key should remain unchanged via GET /organizations/:org/clients/:client" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should include(client['public_key'])
    end
    it "should leave the default key from the keys API list unmodified for that client" do
      list_client_keys($org_name, client['name'], superuser).should include("default")
    end
  end

  context "when a user and client with the same name exist", :authentication do
    before(:each) do
      # give user same name as client
      delete("#{platform.server}/users/#{user['name']}", superuser)
      user['name'] = client['name']
      # post a user with the same name as the client, but with the other public key
      payload = {
        "username" => client['name'],
        "first_name" => client['name'],
        "middle_name" => client['name'],
        "last_name" => client['name'],
        "display_name" => client['name'],
        "email" => "#{client['name']}@#{client['name']}.com",
        "password" => "client-password",
        "public_key" => user['public_key']
      }
      post("#{platform.server}/users", superuser, :payload => payload)
    end
    after do
      post("#{platform.server}/users/#{client['name']}", superuser)
    end
    # note that clients cannot read from /users/:user by default
    it "should not allow client to query /users/:user/keys" do
      # TODO this may be a bit off - we're saying 'client, as the client, should not be allowed to...' which should be 403
      # 401 means that this isn't a valid client - something I'd expect to see if we did client['name'] w/ user['private_key'] and vice-versa
      get("#{platform.server}/users/#{user['name']}", requestor(client['name'], client['private_key'])).should look_like({:status => 401})
    end
    it "should allow user to query /users/:user/keys" do
      get("#{platform.server}/users/#{user['name']}", requestor(client['name'], user['private_key'])).should look_like({:status => 200})
    end
  end

  context "managing keys" do
    shared(:name_suffix) { "#{Time.now.to_i}" }
    shared(:org_admin_name) {"pedant-keys-admin-#{name_suffix}" }
    shared(:org_admin_user) {requestor(org_admin_name, keys[:org_admin][:private]) }
    shared(:org_user_name) {"pedant-keys-user-#{name_suffix}" }
    shared(:org_user) {requestor(org_user_name, keys[:org_user][:private]) }
    shared(:org_client_name) {"pedant-keys-client-#{name_suffix}" }
    shared(:org_client) {requestor(org_client_name, keys[:org_client][:private]) }

    shared(:other_org_name) { "pedant-keys-org-2-#{Time.now.to_i}" }
    shared(:other_org_user_name) { "#{other_org_name}-user" }
    shared(:other_org_user) {requestor(other_org_user_name, keys[:other_org_user][:private]) }
    shared(:other_org_client_name) { "#{other_org_name}-client" }
    shared(:other_org_client) {requestor(other_org_client_name, keys[:other_org_client][:private]) }
    shared(:other_org_user_payload) do
      {
        "public_key" => keys[:other_org_user][:public],
      }
    end
    shared(:org_client_payload) do
      {
        "name" => org_client_name,
        "public_key" => keys[:org_client][:public],
      }
    end

    shared(:org_admin_payload) do
    {
      "public_key" => keys[:org_admin][:public],
    }
    end
    shared(:org_user_payload) do
      {
        "public_key" => keys[:org_user][:public],
      }
    end

    let (:key_payload) do
      {
        "public_key" => keys[:alt_key][:public],
        "name" => "alt_key",
        "expiration_date" => "2100-12-31T23:59:59Z"
      }
    end
    let (:unassociated_requestor) do
      requestor(user['name'], user['private_key'])
    end
    let (:other_org_client_payload) do
      {
        "public_key" => keys[:other_org_client][:public],
        "name" => other_org_client_name
      }
    end

    before :all do
      # This user is used in more tests than not, so create it for the full context.
      # Leave it up to the tests to determine if they need it associated
      platform.create_min_user(org_user_name, overrides: org_user_payload).should look_like({:status => 201} )
      platform.create_min_user(org_admin_name, overrides: org_admin_payload).should look_like({:status => 201} )
      platform.associate_user_with_org($org_name, org_admin_user)
      platform.add_user_to_group($org_name, org_admin_user, "admins")
      platform.create_org(other_org_name)
      platform.create_min_user(other_org_user_name, overrides: other_org_user_payload).should look_like({:status => 201})
      platform.associate_user_with_org(other_org_name, other_org_user)
    end

    after :all do
      platform.remove_user_from_group($org_name, org_admin_user, "admins", superuser)
      platform.delete_user(org_admin_user)
      platform.delete_user(org_user)
      platform.delete_user(other_org_user)
      platform.delete_org(other_org_name)
    end

    context "posting keys" do

      # These behaviors are identical for POSTing to create a client or user key
      shared_context "basic keys POST validation" do
        # Generate validation tests
        { "when name is empty" => {:replace => {"name" => ""}, :response_code => 400 },
          "when name is invalid" => {:replace => {"name" => "key the first"}, :response_code => 400 },
          "when name is missing" => {:delete => ["name"], :response_code => 400},
          "when date is invalid" => {:replace => {"expiration_date" => "2010-09-32T10:00:00Z"}, :response_code => 400},
          "when date is empty" => {:replace => {"expiration_date" => ""}, :response_code => 400},
          "when date is missing" => {:delete =>  ["expiration_date"], :response_code => 400},
          "when public key is not a valid key" => {:replace => { "public_key" => "Nope."}, :response_code => 400},
          "when public key is missing" => {:delete=> ["public_key"], :response_code => 400},
         # "when a key of the same name already exists" => {:replace => {"name" => "default"}, :response_code => 409}
        }.each do |desc, setup|
          it "#{desc} it responds with #{setup[:response_code]}" do
            setup = {:replace=>{}, :delete => []}.merge(setup)
            payload = key_payload.dup
            payload.merge!(setup[:replace])
            setup[:delete].each { |field| payload.delete(field) }
            post(key_url, superuser, :payload => payload).should look_like({:status => setup[:response_code]})
            if setup[:response_code] == 201
              delete("#{key_url}/#{payload['name']}", superuser).should look_like({:code => 200})
            end
          end
        end
        context "when all fields are present and valid" do
          after do
            delete("#{key_url}/#{key_payload['name']}", superuser).should look_like({:status => 200})
          end
          it "should create a key with proper response and Location header" do
            expected_location = "#{key_url}/#{key_payload['name']}"
            response = post("#{key_url}", superuser, :payload => key_payload)
            response.should look_like(
              {
                :status => 201,
                :body_exact => { "uri" => expected_location },
                :headers => [ "Location" => expected_location ]
              })
          end
          it "and infinity date is specfied it should still create a key with proper response and Location header" do
            expected_location = "#{key_url}/#{key_payload['name']}"
            payload = key_payload
            key_payload["expiration_date"] = "infinity"
            response = post("#{key_url}", superuser, :payload => payload)
            response.should look_like(
              {
                :status => 201,
                :body_exact => { "uri" => expected_location },
                :headers => [ "Location" => expected_location ]
              })
          end
        end

      end

      context "for a client" do
        before(:each) do
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end
        after(:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
        end

        let (:key_url) { "#{org_base_url}/clients/#{org_client_name}/keys" }
        it_behaves_like "basic keys POST validation"


        it "that doesn't exist" do
            post("#{org_base_url}/clients/bob/keys", superuser, :payload => key_payload).should look_like({:status => 404})
        end

        context "as...", :authorization do
          it "an invalid user fails with 401", :authentication do
            post("#{org_base_url}/clients/#{org_client_name}/keys", requestor("bob", user['private_key']), :payload => key_payload).should look_like({:status => 401})
          end
          it "the org client itself succeeds" do
            post("#{org_base_url}/clients/#{org_client_name}/keys", org_client, :payload => key_payload).should look_like({:status => 201})
          end
          context "another user in the org" do
            before do
              platform.associate_user_with_org($org_name, org_user).should look_like({:status => 201})
            end
            after do
              platform.remove_user_from_org($org_name, org_user)
            end
            it "fails with 403" do
              post("#{org_base_url}/clients/#{org_client_name}/keys", org_user, :payload => key_payload).should look_like({:status =>  403})
            end
          end
          it "the superuser succeeds" do
            post("#{org_base_url}/clients/#{org_client_name}/keys", superuser, :payload => key_payload).should look_like({:status => 201})
          end
          context "as an org admin of a member org" do
            it "succeeds" do
              post("#{org_base_url}/clients/#{org_client_name}/keys", org_admin_user, :payload => key_payload).should look_like({:status => 201})
            end
          end
        end
      end

      context "for a user" do
        let (:key_url) { "#{platform.server}/users/#{org_user_name}/keys" }
        it_behaves_like "basic keys POST validation"

        it "that doesn't exist" do
            post("#{platform.server}/users/bob/keys", superuser, :payload => key_payload).should look_like({:status => 404})
        end
        context "as...", :authorization do
          before (:all) do
            platform.associate_user_with_org($org_name, org_user).should look_like({:status => 201})
          end
          after (:all) do
            platform.remove_user_from_org($org_name, org_user).should look_like({:status => 200})
          end
          before (:each) do
            post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
          end
          after (:each) do
            delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
            delete_user_key(org_user_name, key_payload["name"])
          end
          it "an invalid user fails with 401", :authentication do
            post("#{platform.server}/users/#{org_user_name}/keys", requestor("bob", user['private_key']), :payload => key_payload).should look_like({:status => 401})
          end
          it "an org client of a member org fails with 401", :authentication do
            post("#{platform.server}/users/#{org_user_name}/keys", org_client, :payload => key_payload).should look_like({:status => 401})
          end
          it "the user itself succeeds" do
            post("#{platform.server}/users/#{org_user_name}/keys", org_user, :payload => key_payload).should look_like({:status => 201})
          end
          it "the superuser succeeds" do
            post("#{platform.server}/users/#{org_user_name}/keys", superuser, :payload => key_payload).should look_like({:status => 201})
          end
          context "an org admin of a member org" do
            it "fails with 403" do
              post("#{platform.server}/users/#{org_user_name}/keys", org_admin_user, :payload => key_payload).should look_like({:status => 403})
            end
          end
        end
      end
    end

    context "when deleting", :authorization do
      context "client keys" do
        before(:all) do
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end
        after(:all) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
        end
        before (:each) do
          add_client_key($org_name, org_client_name, :alt_key, "alt_key").should look_like({:status=>201})
        end
        after (:each) do
          delete_client_key($org_name, org_client_name, "alt_key")
        end
        context "and the actor does not exist" do
          it "DELETE /organizations/:org/clients/bobclient/keys/default fails with 404" do
            delete("#{org_base_url}/clients/bobclient/keys/default", superuser).should look_like({:status => 404})
          end
        end

        context "and the key does not exist" do
          it "DELETE /organizations/:org/clients/:client/keys/badkeyi fails with 404" do
            delete("#{org_base_url}/clients/#{org_client_name}/keys/badkey", superuser).should look_like({:status => 404})
          end
        end

        context "DELETE /organizations/:org/clients/:client/keys/:name as..." do
          it "the client itself, authenticating with a different key should succeed" do
            delete_client_key($org_name, org_client_name, "alt_key",  requestor: org_client).should look_like({:status => 200})
          end

          it "the client itself, authenticating with the key it is trying to delete should fail with 403" do
            r = delete_client_key($org_name, org_client_name, "alt_key",  requestor: requestor(org_client_name, keys[:alt_key][:private]))
            r.should look_like({:status => 403,
                                :body_exact => { "error" => "The key 'alt_key' was used to authenticate this request and cannot be modified or deleted."}})
          end

          context "a client in the same org" do
            before do
              post("#{org_base_url}/clients", superuser, payload: other_org_client_payload ).should look_like({:status => 201})
            end
            after do
              delete("#{org_base_url}/clients/#{other_org_client_name}", superuser).should look_like({:status => 200})
            end
            it "should fail with a 403" do
              delete_client_key($org_name, org_client_name, "alt_key", requestor: other_org_client).should look_like({:status => 403})
            end
          end

          context "a client in a different org", :authentication do
            before do
              post("#{platform.server}/organizations/#{other_org_name}/clients", superuser, payload: other_org_client_payload).should look_like({:status =>201})
            end
            after do
              delete("#{platform.server}/organizations/#{other_org_name}/clients/#{other_org_client_name}", superuser).should look_like({:status =>200})
            end
            it "should fail with a 401" do
              delete_client_key($org_name, org_client_name, "alt_key", requestor: other_org_client).should look_like({:status => 401})
            end
          end

          context "a user in the same org" do
            before do
              platform.associate_user_with_org($org_name, org_user).should look_like({:status => 201})
            end
            after do
              platform.remove_user_from_org($org_name, org_user)
            end
            it "should fail with a 403" do
              delete_client_key($org_name, org_client_name, "alt_key", requestor: org_user).should look_like({:status => 403})
            end

          end

          it "a user not affiliated with the org should fail with a 403" do
            delete_client_key($org_name, org_client_name, "alt_key", requestor: other_org_user).should look_like({:status => 403})
          end

          it "the org admin should succeed" do
            delete_client_key($org_name, org_client_name, "alt_key", requestor: org_admin_user).should look_like({:status => 200})
          end
        end
      end
      context "user keys" do
        before(:all) do
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end
        after(:all) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
        end
        before (:each) do
          add_user_key(org_user_name, :alt_key, "alt_key", :expires =>  unexpired_date).should look_like({:status=>201})
        end
        after (:each) do
          delete_user_key(org_user_name, "alt_key")
        end
        context "and the user does not exist" do
          it "DELETE /users/bob/keys/default" do
            delete("#{platform.server}/users/bob/keys/default", superuser).should look_like({:status => 404})
          end
        end

        context "and the key does not exist" do
          it "DELETE /users/:user/keys/badkey fails with 404" do
            delete("#{platform.server}/users/#{org_user_name}/keys/badkey", superuser).should look_like({:status => 404})
          end
        end

        context "DELETE /users/:user/keys/:name as..." do
          it "the user itself, authenticating with a different key should succeed" do
            delete_user_key(org_user_name, "alt_key",  requestor: org_user).should look_like({:status => 200})
          end

          it "the user itself, authenticating with the key it is trying to delete should fail with 403" do
            r = delete_user_key(org_user_name, "alt_key",  requestor: requestor(org_user_name, keys[:alt_key][:private]))
            r.should look_like({:status => 403,
                                :body_exact => { "error" => "The key 'alt_key' was used to authenticate this request and cannot be modified or deleted."}})
          end

          context "a client in the same org" do
            it "should fail with a 401" do
              delete_user_key(org_user_name, "alt_key", requestor: org_client).should look_like({:status => 401})
            end
          end

          context "a client in a different org", :authentication do
            before do
              post("#{platform.server}/organizations/#{other_org_name}/clients", superuser, payload: other_org_client_payload).should look_like({:status =>201})
            end
            after do
              delete("#{platform.server}/organizations/#{other_org_name}/clients/#{other_org_client_name}", superuser).should look_like({:status =>200})
            end
            it "should fail with a 401" do
              delete_user_key(org_user_name, "alt_key", requestor: other_org_client).should look_like({:status => 401})
            end
          end

          context "a user in the same org" do
            before do
              platform.associate_user_with_org($org_name, other_org_user).should look_like({:status => 201})
            end
            after do
              platform.remove_user_from_org($org_name, other_org_user).should look_like({:status => 200})
            end
            it "should fail with a 403" do
              delete_user_key(org_user_name, "alt_key", requestor: other_org_user).should look_like({:status => 403})
            end
          end

          it "a user not affiliated with the org should fail with a 403" do
            delete_user_key(org_user_name, "alt_key", requestor: other_org_user).should look_like({:status => 403})
          end

          it "the org admin should fail with a 403" do
            delete_user_key(org_user_name, "alt_key", requestor: org_admin_user).should look_like({:status => 403})
          end
        end
      end
    end


    context "listing key(s)" do
      context "when multiple keys are present" do
        context "for a client" do
          before(:each) do
            post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
            add_client_key($org_name, org_client_name, :key, "key1", :expires =>  unexpired_date).should look_like({:status=>201})
            add_client_key($org_name, org_client_name, :alt_key, "key2", :expires => expired_date).should look_like({:status=>201})
          end
          after(:each) do
            delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          end
          context "when GET /organizations/org/clients/client/keys is called (list keys)" do
            it "all keys should be listed with correct expiry indicators" do
              list_client_keys($org_name, org_client_name, superuser).should look_like({
                :status => 200,
                :body_exact => [
                  { "name" => "default", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/default", "expired" => false },
                  { "name" => "key1", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/key1", "expired" => false },
                  { "name" => "key2", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/key2", "expired" => true}
                ]})
            end
            context "when GET is called on the URIs that are returned" do
              it "should return status 200" do
                client_keys = list_client_keys($org_name, org_client_name, superuser)
                JSON.parse(client_keys).each do |key|
                  get(key["uri"], superuser).should look_like({:status => 200})
                end
              end
            end
          end
          context "when GET /organizations/org/clients/client/keys/key is called (get single key)" do
            context "when it is called for each valid key" do
              it "should properly return the default key with valid expiration" do
                get_client_key($org_name, org_client_name, superuser, "default").should look_like({
                  :status => 200,
                  :body => { "name" => "default", "public_key" => keys[:org_client][:public], "expiration_date" => "infinity" }
                  })
              end
              it "should properly return the first custom key with valid expiration" do
                get_client_key($org_name, org_client_name, superuser, "key1").should look_like({
                  :status => 200,
                  :body => { "name" => "key1", "public_key" => keys[:key][:public], "expiration_date" =>  unexpired_date}
                  })
              end
              it "should properly return the second custom key with valid expiration" do
                get_client_key($org_name, org_client_name, superuser, "key2").should look_like({
                  :status => 200,
                  :body => { "name" => "key2", "public_key" => keys[:alt_key][:public], "expiration_date" => expired_date }
                  })
              end
            end
          end
        end

        context "for a user" do
          before(:each) do
            platform.associate_user_with_org($org_name, org_user).should look_like({:status => 201})
            add_user_key(org_user_name, :key, "key1", :expires => unexpired_date).should look_like({:status => 201})
            add_user_key(org_user_name, :alt_key, "key2", :expires => expired_date).should look_like({:status => 201})
          end
          after(:each) do
            platform.remove_user_from_org($org_name, org_user).should look_like({:status => 200})
            delete_user_key(org_user_name, "key1").should look_like({:status => 200})
            delete_user_key(org_user_name, "key2").should look_like({:status => 200})

          end

          context "when GET /users/user/keys is called (list keys)" do
            it "all keys should be listed with correct expiry indicators" do
              list_user_keys(org_user_name, superuser).should look_like({
                :status => 200,
                :body => [
                  { "name" => "default", "uri" => "#{platform.server}/users/#{org_user_name}/keys/default", "expired" => false },
                  { "name" => "key1", "uri" => "#{platform.server}/users/#{org_user_name}/keys/key1", "expired" => false },
                  { "name" => "key2", "uri" => "#{platform.server}/users/#{org_user_name}/keys/key2", "expired" => true }
                ]})
            end
            context "when GET is called on the URIs that are returned" do
              it "should return status 200" do
                user_keys = list_user_keys(org_user_name, superuser)
                JSON.parse(user_keys).each do |key|
                  get(key["uri"], superuser).should look_like({:status => 200})
                end
              end
            end
          end

          context "when GET /users/user/keys/key is called (get single key)" do
            context "when it is called for each valid key" do
              it "should properly return the default key with valid expiration" do
                get_user_key(org_user_name, superuser, "default").should look_like({
                  :status => 200,
                  :body => { "name" => "default", "public_key" => keys[:org_user][:public], "expiration_date" => "infinity" }
                  })
              end
              it "should properly return the first custom key with valid expiration" do
                get_user_key(org_user_name, superuser, "key1").should look_like({
                  :status => 200,
                  :body => { "name" => "key1", "public_key" => keys[:key][:public], "expiration_date" => unexpired_date }
                  })
              end
              it "should properly return the second custom key with valid expiration" do
                get_user_key(org_user_name, superuser, "key2").should look_like({
                  :status => 200,
                  :body => { "name" => "key2", "public_key" => keys[:alt_key][:public], "expiration_date" => expired_date }
                  })
              end
            end
          end
        end
      end

      context "of a user" do
        before(:each) do
          platform.associate_user_with_org($org_name, org_user).should look_like({:status => 201})
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end

        after (:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          platform.remove_user_from_org($org_name, org_user).should look_like({:status => 200})
        end

        context "when GET /users/user/keys is called (list keys)" do
          it "by an invalid user fails with a 401", :authentication do
            get("#{platform.server}/users/#{user['name']}/keys", requestor("bob", user['private_key'])).should look_like({:status => 401})
          end
          it "by a client in the same org fails with a 401", :authentication do
            list_user_keys(org_user_name, org_client).should look_like({:status => 401})
          end
          it "who isn't valid by a user who is valid fails with a 404" do
            get("#{platform.server}/users/bob/keys", org_user).should look_like({:status => 404})
          end
        end

        context "when GET /users/user/keys/default is called" do
          it "by an invalid user fails with a 401", :authentication do
            get("#{platform.server}/users/#{user['name']}/keys/default", requestor("bob", user['private_key'])).should look_like({:status => 401})
          end
          it "by a client in the same org fails with a 401", :authentication do
            get_user_key(org_user_name, org_client, "default").should look_like({:status => 401})
          end
          it "who isn't valid by a user who is valid fails with a 404" do
            get("#{platform.server}/users/bob/keys/default", org_user).should look_like({:status => 404})
          end
        end
      end

      context "by an org admin", :authorization do
        before(:each) do
          platform.associate_user_with_org($org_name, org_user)
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end

        after(:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          platform.remove_user_from_org($org_name, org_user).should look_like({:status => 200})
        end

        context "when GET /organizations/org/clients/client/keys is called" do
          it "for a client that is a member of the same org succeeds with a 200" do
            list_client_keys($org_name, org_client_name, org_admin_user).should look_like({:status => 200})
          end

          it "for a client that is a member of a different org fails with a 403" do
            list_client_keys(other_org_name, other_org_client_name, org_admin_user).should look_like({:status => 403})
          end
        end

        context "when GET /users/user/keys is called" do
          it "for a user that is a member of the same org succeeds with a 200" do
            list_user_keys(org_user_name, org_admin_user).should look_like({:status => 200})
          end

          it "for a user that is not a member of the same org fails with a 403" do
            list_user_keys(other_org_user_name, org_admin_user).should look_like({:status => 403})
          end
        end

        context "when GET /organizations/org/clients/client/keys/key is called" do
          it "for a client that is a member of the same org succeeds with a 200" do
            get_client_key($org_name, org_client_name, org_admin_user, "default").should look_like({:status => 200})
          end

          it "for a client that is a member of a different org fails with a 403" do
            get_client_key(other_org_name, other_org_client_name, org_admin_user, "default").should look_like({:status => 403})
          end
        end

        context "when GET /users/user/keys/key is called" do
          it "for a user that is a member of the same org succeeds with a 200" do
            get_user_key(org_user_name, org_admin_user, "default").should look_like({:status => 200})
          end

          it "for a user that is not a member of the same org fails with a 403" do
            get_user_key(other_org_user_name, org_admin_user, "default").should look_like({:status => 403})
          end
        end

      end

      context "by an org client", :authorization do
        before (:each) do
          platform.associate_user_with_org($org_name, org_user)
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end

        after (:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          platform.remove_user_from_org($org_name, org_user).should look_like({:status => 200})
        end

        context "when GET /organizations/org/clients/client/keys is called" do
          it "for a client that is a member of the same org fails with a 403" do
            list_client_keys($org_name, client['name'], org_client).should look_like({:status => 403})
          end
          it "for a itself succeeds with a 200" do
            list_client_keys($org_name, org_client_name, org_client).should look_like({:status => 200})
          end
          it "for a client that is a member of a different org fails with a 401" do
            list_client_keys(other_org_name, other_org_client_name, org_client).should look_like({:status => 401})
          end
        end

        context "when GET /users/user/keys is called" do
          it "for a user that is a member of the same org fails with a 401" do
            list_user_keys(org_user_name, org_client).should look_like({:status => 401})
          end

          it "for a user that is not a member of the same org fails with a 401" do
            list_user_keys(other_org_user_name, org_client).should look_like({:status => 401})
          end
        end

        context "when GET /organizations/org/clients/client/keys/key is called" do
          it "for a client that is a member of the same org fails with a 403" do
            get_client_key($org_name, client['name'], org_client, "default").should look_like({:status => 403})
          end
          it "for a itself succeeds with a 200" do
            get_client_key($org_name, org_client_name, org_client, "default").should look_like({:status => 200})
          end
          it "for a client that is a member of a different org fails with a 401" do
            get_client_key(other_org_name, other_org_client_name, org_client, "default").should look_like({:status => 401})
          end
        end

        context "when GET /users/user/keys/key is called" do
          it "for a user that is a member of the same org fails with a 401" do
            get_user_key(org_user_name, org_client, "default").should look_like({:status => 401})
          end

          it "for a user that is not a member of the same org fails with a 401" do
            get_user_key(other_org_user_name, org_client, "default").should look_like({:status => 401})
          end
        end
      end

      context "by an org member who is not an admin", :authorization do
        before (:each) do
          platform.associate_user_with_org($org_name, org_user)
          platform.associate_user_with_org(other_org_name, other_org_user)
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end
        after (:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          platform.remove_user_from_org($org_name, org_user).should look_like({:status => 200})
          platform.remove_user_from_org(other_org_name, other_org_user)
        end

        context "when GET /organizations/org/clients/client/keys is called" do
          it "for a client that is a member of the same org succeeds with a 200" do
            list_client_keys($org_name, org_client_name, org_user).should look_like({:status => 200})
          end
          it "for a client that is a member of a different org fails with a 403" do
            list_client_keys($org_name, org_client_name, other_org_user).should look_like({:status => 403})
          end
        end

        context "when GET /users/user/keys is called" do
          it "for a user that is a member of the same org fails with a 403" do
            list_user_keys(user['name'], org_user).should look_like({:status => 403})
          end
          it "for a user that is not a member of the same org fails with a 403" do
            list_user_keys(org_user_name, other_org_user).should look_like({:status => 403})
          end
        end

        context "when GET /organizations/org/clients/client/key is called" do
          it "for a client that is a member of the same org succeeds with a 200" do
            get_client_key($org_name, org_client_name, org_user, "default").should look_like({:status => 200})
          end
          it "for a client that is a member of a different org fails with a 403" do
            get_client_key($org_name, org_client_name, other_org_user, "default").should look_like({:status => 403})
          end
        end

        context "when GET /users/user/keys/key is called" do
          it "for a user that is a member of the same org fails with a 403" do
            get_user_key(user['name'], org_user, "default").should look_like({:status => 403})
          end
          it "for a user that is not a member of the same org fails with a 403" do
            get_user_key(org_user_name, other_org_user, "default").should look_like({:status => 403})
          end
        end
      end

      context "by an unaffiliated user", :authorization do
        before (:each) do
          post("#{platform.server}/organizations/#{other_org_name}/clients", superuser, :payload => other_org_client_payload).should look_like({:status => 201})
        end
        after (:each) do
          delete("#{platform.server}/organizations/#{other_org_name}/clients/#{other_org_client_name}", superuser).should look_like({:status => 200})
        end
        context "when GET /organizations/org/clients/client/keys is called" do
          it "attempting to see an org client's keys fails with a 403" do
            list_client_keys(other_org_name, other_org_client_name, org_user).should look_like({:status => 403})
          end
        end

        context "when GET /users/user/keys is called" do
          it "attempting to see their own keys succeeds with a 200" do
            list_user_keys(org_user_name, org_user).should look_like({:status => 200})
          end
          it "attempting to see someone else's keys fails with a 403" do
            list_user_keys(other_org_user_name, org_user).should look_like({:status => 403})
          end
        end

        context "when GET /organizations/org/clients/client/keys/key is called" do
          it "attempting to see an org client's key fails with a 403" do
            get_client_key(other_org_name, other_org_client_name, org_user, "default").should look_like({:status => 403})
          end
        end

        context "when GET /users/user/keys/key is called" do
          it "attempting to see their own keys succeeds with a 200" do
            get_user_key(org_user_name, org_user, "default").should look_like({:status => 200})
          end
          it "attempting to see someone else's key fails with a 403" do
            get_user_key(other_org_user_name, org_user, "default").should look_like({:status => 403})
          end
        end
      end
    end
  end
end
