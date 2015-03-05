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


# TODO WIP All this can be replaced by API instead of ctl once
# the API is implemented.
#
# Anywhere you see "API" in the rspec descriptions and contexts are currently using
# ctl and should be updated to use the API.

require 'json'

describe "/keys endpoint", :keys do

  # Noise reducer.
  def requestor(who, key)
    Pedant::Requestor.new(who, key)
  end

  let(:keys) {@keys}
  let(:user) do
    {
      "name" => "pedant-user-#{Time.now.to_i}",
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

  $org = {
      "name" => "pedant-org-#{Time.now.to_i}"
  }

  $org_payload = {
      "name" => $org['name'],
      "full_name" => $org['name']
  }

  let(:client) do
    {
      "name" => "pedant-client-#{Time.now.to_i}",
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

  def org_base_url()
    "#{platform.server}/organizations/#{$org['name']}"
  end

  def delete_client_key(org, client, keyname)
    system("chef-server-ctl delete-client-key #{org} #{client} #{keyname}")
  end

  def delete_user_key(user, keyname)
    system("chef-server-ctl delete-user-key #{user} #{keyname}")
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

  def list_client_keys(org, client, requestor)
    get("#{platform.server}/organizations/#{org}/clients/#{client}/keys", requestor)
  end

  def make_user_payload(payload)
    base_user_payload.dup.merge(payload)
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
          :privkey_file => priv,
          :path => "#{pub.path}",
          :private => File.read(priv.path),
          :public => File.read(pub.path)
           }
        pub.close
        pub.unlink
        end
    rescue Exception => e
      puts "Error creating keys: #{e.message}"
      raise
    end

    # org is static in the tests, only create once
    post("#{platform.server}/organizations", superuser, :payload => $org_payload)
  end

  after(:all) do
    @keys.each do |name, key|
      key[:privkey_file].close
      key[:privkey_file].unlink

    end

    # clean up org
    delete("#{org_base_url}/clients/#{$org['name']}-validator", superuser)
    delete("#{org_base_url}", superuser)
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
      list_client_keys($org['name'], client['name'], superuser).should look_like( { :status=> 200, :body => new_client_list_keys_response} )
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
        delete_user_key(user['name'], "default")
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
        delete_client_key($org['name'], client['name'], "default")
        add_client_key($org['name'], client['name'], :key, "default").should look_like({:status=>201})
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
        add_client_key($org['name'], client['name'], :alt_key, key_name).should look_like({:status=>201})
    end
    it "should not longer be returned by the keys API" do
      delete_client_key($org['name'], client['name'], key_name)
      list_client_keys($org['name'], client['name'], superuser).should_not include(key_name)
    end
    it "should still contain other keys not yet deleted" do
      delete_client_key($org['name'], client['name'], key_name)
      list_client_keys($org['name'], client['name'], superuser).should include("default")
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
        add_client_key($org['name'], client['name'], :alt_key, "alt-#{key_name}").should look_like({:status=>201})
        add_client_key($org['name'], client['name'], :key, key_name).should look_like({:status=>201})
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
      add_user_key(user['name'], :key, key_name, :expires => "2017-12-24T21:00:00").should look_like({:status => 201})
    end
    it "should authenticate against the key" do
      get("#{platform.server}/users/#{user['name']}", requestor(user['name'], keys[:key][:private])).should look_like({:status => 200})
    end
  end

  context "when a user's default key has an expiration date" do
    before(:each) do
      delete_user_key(user['name'], "default")
      add_user_key(user['name'], :key, "default", :expires => "2017-12-24T21:00:00").should look_like({:status => 201})
    end
    context "and is updated via a PUT to /users/:user" do
      before(:each) do
        original_data = JSON.parse(get("#{platform.server}/users/#{user['name']}", superuser))
        original_data['public_key'] = keys[:alt_key][:public]
        put("#{platform.server}/users/#{user['name']}", superuser, :payload => original_data)
      end
      it "will no longer have an expiration date when queried via the keys API" do
        `chef-server-ctl list-user-keys #{user['name']}`.should include("Infinity")
      end
    end
  end

  context "when a client's default key has an expiration date" do
    before(:each) do
      delete_client_key($org['name'], client['name'], "default")
      add_client_key($org['name'], client['name'], :key, "default", :expires => "2017-12-24T21:00:00").should look_like({:status=>201})
    end
    context "and is updated via a PUT to /organizations/:org/clients/:client" do
      before(:each) do
        original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
        original_data['public_key'] = keys[:alt_key][:public]
        put("#{org_base_url}/clients/#{client['name']}", superuser, :payload => original_data)
      end
      it "should no longer have an expiration date when queried via the keys API" do
        `chef-server-ctl list-client-keys #{$org['name']} #{client['name']}`.should include("Infinity")
      end
    end
  end

  context "when a client key has an expiration date and isn't expired" do
    before(:each) do
      add_client_key($org['name'], client['name'], :key, key_name, :expires => "2017-12-24T21:00:00").should look_like({:status=>201})
    end
    it "should authenticate against the key" do
      get("#{org_base_url}/clients/#{client['name']}", requestor(client['name'], keys[:key][:private])).should look_like({:status => 200})
    end
  end

  context "when a key is expired for a user", :authentication do
    before(:each) do
      add_user_key(user['name'], :key, key_name, :expires => "2012-12-24T21:00:00").should look_like({:status => 201})
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
      add_client_key($org['name'], client['name'], :key, key_name, :expires => "2012-12-24T21:00:00" ).should look_like({:status=>201})
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
      `chef-server-ctl list-user-keys #{user['name']}`.should include(user_api_public_key)
    end
  end

  context "when the default key for a client exists" do
    it "should return public_key field returned by GET /organization/:org/clients/:client and from the keys table should be the same" do
      client_api_public_key = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key']
      `chef-server-ctl list-client-keys #{$org['name']} #{client['name']}`.should include(client_api_public_key)
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
      delete_client_key($org['name'], client['name'], "default")
    end

    it "public field returned by /organizations/:org/clients/:client should be null" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should == nil
    end
    it "the keys API should not return a key named default" do
      list_client_keys($org['name'], client['name'], superuser).should_not include("default")
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
    it "should not modify the default key returned via the keys API" do
      `chef-server-ctl list-user-keys #{user['name']}`.should include(user['public_key'])
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
    it "should not modify the default key returned via the keys API" do
      `chef-server-ctl list-client-keys #{$org['name']} #{client['name']}`.should include(client['public_key'])
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
      delete_client_key($org['name'], client['name'], "default")
      original_data = JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))
      original_data['public_key'] = keys[:key][:public]
      put("#{org_base_url}/clients/#{client['name']}", superuser, :payload => original_data)
    end
    it "should be shown in the clients's record via GET of the named client" do
      JSON.parse(get("#{org_base_url}/clients/#{client['name']}", superuser))['public_key'].should include(keys[:key][:public])
    end
    it "should be present in the keys list" do
      list_client_keys($org['name'], client['name'], superuser).should include("default")
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
        `chef-server-ctl list-user-keys #{user['name']}`.should include(keys[:key][:public])
      end
      it "should no longer contain the old default key" do
        `chef-server-ctl list-user-keys #{user['name']}`.should_not include(user['public_key'])
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
        `chef-server-ctl list-client-keys #{$org['name']} #{client['name']}`.should include(keys[:key][:public])
      end
      it "should no longer contain the old default key" do
        `chef-server-ctl list-client-keys #{$org['name']} #{client['name']}`.should_not include(user['public_key'])
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
      list_client_keys($org['name'], client['name'], superuser).should include("default")
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
    # TODO Consider making these globals instead of lets - it's painfully slow to have this, and org association re-run
    # with every example, and since we don't change the data we care about in these tests, there's no real benefit in terms
    # of having these users/clients/associations recreated per test.
    let (:name_suffix) { "#{Time.now.to_i}" }
    let (:org_admin_name) {"admin-#{name_suffix}" }
    let (:org_admin) {requestor(org_admin_name, keys[:org_admin][:private]) }
    let (:org_user_name) {"user-#{name_suffix}" }
    let (:org_user) {requestor(org_user_name, keys[:org_user][:private]) }
    let (:org_client_name) {"client-#{name_suffix}" }
    let (:org_client) {requestor(org_client_name, keys[:org_client][:private]) }

    $other_org_name = "other-org-#{Time.now.to_i}"
    $other_org_payload =  { "name" => $other_org_name, "full_name" => $other_org_name }

    # Here we can re-use the our primary user/clients, because they are nor members of
    # other-org
    let (:other_org_user_name) { "#{$other_org_name}-user" }
    let (:other_org_user) {requestor(other_org_user_name, keys[:other_org_user][:private]) }
    let (:other_org_client_name) { "#{$other_org_name}-client" }
    let (:other_org_client) {requestor(non_org_client_name, keys[:other_org_client][:private]) }

    let (:base_user_payload) do
      {
        "first_name" => "Do",
        "middle_name" => "Not",
        "last_name" => "Care",
        "display_name" => "Really Do Not",
        "password" => "client-password",
      }

    end
    let (:org_admin_payload) do
    {
      "public_key" => keys[:org_admin][:public],
      "username" => org_admin_name,
      "email" => "#{org_admin_name}@#{org_admin_name}.com"
    }
    end
    let (:org_user_payload) do
      {
        "public_key" => keys[:org_user][:public],
        "username" => org_user_name,
        "email" => "#{org_user_name}@#{org_user_name}.com"
      }
    end
    let (:other_org_user_payload) do
      {
        "public_key" => keys[:other_org_user][:public],
        "username" => other_org_user_name,
        "email" => "#{other_org_user_name}@#{other_org_user_name}.com"
      }
    end
    let (:org_client_payload) do
      {
        "public_key" => keys[:org_client][:public],
        "name" => org_client_name,
        "admin" =>false
      }
    end
    let (:key_payload) do
      {
        "public_key" => keys[:alt_key][:public],
        "name" => "alt_key",
        "expiration_date" => "2100-12-31T23:59:59"
      }
    end

    let (:other_org_client_payload) do
      {
        "public_key" => keys[:other_org_client][:public],
        "name" => other_org_client_name,
        "admin" =>false
      }
    end
    let (:unassociated_requestor) do
      requestor(user['name'], user['private_key'])
    end

    before :all do
      # Create empty org
      post("#{platform.server}/organizations", superuser, :payload => $other_org_payload).should look_like({:status => 201})
    end
    after :all do
      delete("#{platform.server}/organizations/#{$other_org_name}/clients/#{$other_org_name}-validator", superuser).should look_like({:status => 200})
      delete("#{platform.server}/organizations/#{$other_org_name}", superuser).should look_like({:status => 200})
    end

    context "posting keys" do

      # These behaviors are identical for POSTing to create a client or user key
      shared_context "basic keys POST validation" do
        # Generate validation tests
        { "when name is empty" => {:replace => {"name" => ""}, :response_code => 400 },
          "when name is invalid" => {:replace => {"name" => "key the first"}, :response_code => 400 },
          "when name is missing" => {:delete => ["name"], :response_code => 400},
          "when date is invalid" => {:replace => {"expiration_date" => "2010-09-32T10:00:00"}, :response_code => 400},
          "when date is infinity" => {:replace => {"expiration_date" => "infinity" }, :response_code => 201},
          "when date is empty" => {:replace => {"expiration_date" => ""}, :response_code => 400},
          "when date is missing" => {:delete =>  ["expiration_date"], :response_code => 400},
          "when public key is not a valid key" => {:replace => { "public_key" => "Nope."}, :response_code => 400},
          "when public key is missing" => {:delete=> ["public_key"], :response_code => 400},
          "when a key of the same name already exists" => {:replace => {"name" => "default"}, :response_code => 409}
        }.each do |desc, setup|
          it "#{desc} it responds with #{setup[:response_code]}" do
            payload = key_payload.dup
            payload = payload.merge(setup[:replace]) if setup[:replace]
            setup[:delete] and setup[:delete].each { |field| payload.delete(field) }
            post(key_url, superuser, :payload => payload).should look_like({:status => setup[:response_code]})
          end
        end
        context "when all fields are present and valid" do
          it "should create a key and with proper response and Location header" do
            expected_location = "#{key_url}/#{key_payload['name']}"
            response = post("#{key_url}", superuser, :payload => key_payload)
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
          context "an unrelated user" do
            before do
              post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201})
              post("#{org_base_url}/users", superuser, :payload => { "username" => org_user_name} ).should look_like({:status => 201})
            end
            after do
              delete("#{org_base_url}/users/#{org_user_name}", superuser).should look_like({:status => 200})
              delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
            end
            it "fails with 403" do
              post("#{org_base_url}/clients/#{org_client_name}/keys", org_user, :payload => key_payload).should look_like({:status =>  403})
            end
          end
          it "the superuser succeeds" do
            post("#{org_base_url}/clients/#{org_client_name}/keys", superuser, :payload => key_payload).should look_like({:status => 201})
          end
          context "as an org admin of a member org" do
            before(:each) do
              post("#{platform.server}/users", superuser, :payload => make_user_payload(org_admin_payload)).should look_like({:status => 201} )
              post("#{org_base_url}/users", superuser, :payload => { "username" => org_admin_name} ).should look_like({:status => 201})
              platform.add_user_to_group($org['name'], org_admin, "admins")
            end
            after(:each) do
              platform.remove_user_from_group($org['name'], org_admin, "admins", superuser)
              delete("#{org_base_url}/users/#{org_admin_name}", superuser).should look_like({:status => 200})
              delete("#{platform.server}/users/#{org_admin_name}", superuser).should look_like({:status => 200})
            end
            it "succeeds" do
              post("#{org_base_url}/clients/#{org_client_name}/keys", org_admin, :payload => key_payload).should look_like({:status => 201})
            end
          end
        end
      end

      context "for a user" do
        before(:each) do
          post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201})
        end
        after(:each) do
          delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
        end
        let (:key_url) { "#{platform.server}/users/#{org_user_name}/keys" }
        it_behaves_like "basic keys POST validation"

        it "that doesn't exist" do
            post("#{platform.server}/users/bob/keys", superuser, :payload => key_payload).should look_like({:status => 404})
        end
        context "as...", :authorization do
          before (:each) do
            post("#{org_base_url}/users", superuser, :payload => { "username" => org_user_name} ).should look_like({:status => 201})
            post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
          end
          after (:each) do
            delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
            delete("#{org_base_url}/users/#{org_user_name}", superuser).should look_like({:status => 200})
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
          context "as an org admin of a member org" do
            before(:each) do
              post("#{platform.server}/users", superuser, :payload => make_user_payload(org_admin_payload)).should look_like({:status => 201} )
              post("#{org_base_url}/users", superuser, :payload => { "username" => org_admin_name} ).should look_like({:status => 201})
              platform.add_user_to_group($org['name'], org_admin, "admins")
            end
            after(:each) do
              platform.remove_user_from_group($org['name'], org_admin, "admins", superuser)
              delete("#{org_base_url}/users/#{org_admin_name}", superuser).should look_like({:status => 200})
              delete("#{platform.server}/users/#{org_admin_name}", superuser).should look_like({:status => 200})
            end
            it "fails with 403" do
              post("#{platform.server}/users/#{org_user_name}/keys", org_admin, :payload => key_payload).should look_like({:status => 403})
            end
          end
        end
      end
    end
    context "listing keys" do
      context "when multiple keys are present" do
        context "for a client" do
          before(:each) do
            post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
            add_client_key($org['name'], org_client_name, :key, "key1", :expires => "2017-12-24T21:00" ).should look_like({:status=>201})
            add_client_key($org['name'], org_client_name, :alt_key, "key2", :expires => "2012-01-01T00:00" ).should look_like({:status=>201})
          end
          after(:each) do
            delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          end
          it "all keys should be listed with correct expiry indicators" do
            list_client_keys($org['name'], org_client_name, superuser).should look_like({
              :status => 200,
              :body => [
                { "name" => "default", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/default", "expired" => false },
                { "name" => "key1", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/key1", "expired" => false },
                { "name" => "key2", "uri" => "#{org_base_url}/clients/#{org_client_name}/keys/key2", "expired" => true}
              ]})

          end
        end
        context "for a user" do
          before(:each) do
            post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201} )
            post("#{org_base_url}/users", superuser, :payload => { "username" => org_user_name} ).should look_like({:status => 201})
            add_user_key(org_user_name, :key, "key1", :expires => "2017-12-24T21:00:00").should look_like({:status => 201})
            add_user_key(org_user_name, :alt_key, "key2", :expires => "2012-01-01T00:00").should look_like({:status => 201})
          end
          after(:each) do
            delete("#{org_base_url}/users/#{org_user_name}", superuser).should look_like({:status => 200})
            delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          end
          it "all keys should be listed with correct expiry indicators" do
            list_user_keys(org_user_name, superuser).should look_like({
              :status => 200,
              :body => [
                { "name" => "default", "uri" => "#{platform.server}/users/#{org_user_name}/keys/default", "expired" => false },
                { "name" => "key1", "uri" => "#{platform.server}/users/#{org_user_name}/keys/key1", "expired" => false },
                { "name" => "key2", "uri" => "#{platform.server}/users/#{org_user_name}/keys/key2", "expired" => true }
              ]})

          end
        end
      end
      context "of a user" do
        before(:each) do
          post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201} )
          post("#{org_base_url}/users", superuser, :payload => { "username" => org_user_name} ).should look_like({:status => 201})
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
        end

        after (:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          delete("#{org_base_url}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
        end

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

      context "by an org admin", :authorization do
        before(:each) do
          post("#{platform.server}/users", superuser, :payload => make_user_payload(org_admin_payload)).should look_like({:status => 201} )
          post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201} )
          post("#{platform.server}/users", superuser, :payload => make_user_payload(other_org_user_payload)).should look_like({:status => 201} )
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
          post("#{org_base_url}/users", superuser, :payload => { "username" => org_user_name} ).should look_like({:status => 201})
          post("#{org_base_url}/users", superuser, :payload => { "username" => org_admin_name} ).should look_like({:status => 201})
          post("#{platform.server}/organizations/#{$other_org_name}/users", superuser, :payload => { "username" => other_org_user_name } ).should look_like({:status => 201})
          platform.add_user_to_group($org['name'], org_admin, "admins")
        end

        after(:each) do
          platform.remove_user_from_group($org['name'], org_admin, "admins", superuser)
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          delete("#{org_base_url}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{org_base_url}/users/#{org_admin_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{org_admin_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/organizations/#{$other_org_name}/users/#{other_org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{other_org_user_name}", superuser).should look_like({:status => 200})
        end

        it "for a client that is a member of the same org succeeds with a 200" do
          list_client_keys($org['name'], org_client_name, org_admin).should look_like({:status => 200})
        end

        it "for a client that is a member of a different org fails with a 403" do
          list_client_keys($other_org_name, other_org_client_name, org_admin).should look_like({:status => 403})
        end

        it "for a user that is a member of the same org succeeds with a 200" do
          list_user_keys(org_user_name, org_admin).should look_like({:status => 200})
        end

        it "for a user that is not a member of the same org fails with a 403" do
          list_user_keys(other_org_user_name, org_admin).should look_like({:status => 403})
        end
      end
      context "by an org client", :authorization do
        before (:each) do
          post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201} )
          post("#{platform.server}/users", superuser, :payload => make_user_payload(other_org_user_payload)).should look_like({:status => 201} )
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
          post("#{org_base_url}/users", superuser, :payload => { "username" => org_user_name} ).should look_like({:status => 201})
          post("#{platform.server}/organizations/#{$other_org_name}/users", superuser, :payload => { "username" => other_org_user_name } ).should look_like({:status => 201})
        end

        after (:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          delete("#{org_base_url}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/organizations/#{$other_org_name}/users/#{other_org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{other_org_user_name}", superuser).should look_like({:status => 200})
        end

        it "for a client that is a member of the same org fails with a 403" do
          list_client_keys($org['name'], client['name'], org_client).should look_like({:status => 403})
        end
        it "for a itself succeeds with a 200" do
          list_client_keys($org['name'], org_client_name, org_client).should look_like({:status => 200})
        end

        it "for a client that is a member of a different org fails with a 401" do
          list_client_keys($other_org_name, other_org_client_name, org_client).should look_like({:status => 401})
        end

        it "for a user that is a member of the same org fails with a 401" do
          list_user_keys(org_user_name, org_client).should look_like({:status => 401})
        end

        it "for a user that is not a member of the same org fails with a 401" do
          list_user_keys(other_org_user_name, org_client).should look_like({:status => 401})
        end
      end

      context "by an org member who is not an admin", :authorization do
        before (:each) do
          post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201} )
          post("#{platform.server}/users", superuser, :payload => make_user_payload(other_org_user_payload)).should look_like({:status => 201} )
          post("#{org_base_url}/clients", superuser, :payload => org_client_payload).should look_like({:status => 201})
          post("#{org_base_url}/users", superuser, :payload => { "username" => org_user_name} ).should look_like({:status => 201})
          post("#{platform.server}/organizations/#{$other_org_name}/users", superuser, :payload => { "username" => other_org_user_name } ).should look_like({:status => 201})
        end
        after (:each) do
          delete("#{org_base_url}/clients/#{org_client_name}", superuser).should look_like({:status => 200})
          delete("#{org_base_url}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/organizations/#{$other_org_name}/users/#{other_org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{other_org_user_name}", superuser).should look_like({:status => 200})
        end

        it "for a client that is a member of the same org succeeds with a 200" do
          list_client_keys($org['name'], org_client_name, org_user).should look_like({:status => 200})
        end
        it "for a client that is a member of a different org fails with a 403" do
          list_client_keys($org['name'], org_client_name, other_org_user).should look_like({:status => 403})

        end
        it "for a user that is a member of the same org fails with a 403" do
          list_user_keys(user['name'], org_user).should look_like({:status => 403})
        end
        it "for a user that is not a member of the same org fails with a 403" do
          list_user_keys(org_user_name, other_org_user).should look_like({:status => 403})
        end
      end

      context "by an unaffiliated user", :authorization do
        before (:each) do
          # A note on these: we reuse org_user, but in this case the user does not get associated with an org.
          post("#{platform.server}/organizations/#{$other_org_name}/clients", superuser, :payload => other_org_client_payload).should look_like({:status => 201})
          post("#{platform.server}/users", superuser, :payload => make_user_payload(org_user_payload)).should look_like({:status => 201} )
          post("#{platform.server}/users", superuser, :payload => make_user_payload(other_org_user_payload)).should look_like({:status => 201} )
        end
        after (:each) do
          delete("#{platform.server}/organizations/#{$other_org_name}/clients/#{other_org_client_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{org_user_name}", superuser).should look_like({:status => 200})
          delete("#{platform.server}/users/#{other_org_user_name}", superuser).should look_like({:status => 200})
        end
        it "attempting to see their own keys succeeds with a 200" do
          list_user_keys(org_user_name, org_user).should look_like({:status => 200})
        end
        it "attempting to see someone else's keys fails with a 403" do
          list_user_keys(other_org_user_name, org_user).should look_like({:status => 403})
        end
        it "attempting to see an org client's keys fails with a 403" do
          list_client_keys($other_org_name,  other_org_client_name, org_user).should look_like({:status => 403})
        end
      end
    end
  end
end
