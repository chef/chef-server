require 'pedant/rspec/common'

# Captures behaviors introduced in server api v1 that are not easily
# handled elsewhere.  As v1 becomes the min suppported version, we'll
# need to merge these tests in to replace the ones they supercede
describe "Server API v1 Behaviors", :api_v1 do
  context "[v1+]" do
    shared(:pubkey_regex) { /^(-----BEGIN (RSA )?PUBLIC KEY)/ }
    shared(:privkey_regex) { /^(-----BEGIN (RSA )?PRIVATE KEY)/ }

    shared(:org_name) do
      if Pedant.config[:org][:create_me]
        unique_name("api-v1-org")
      else
        Pedant.config[:org][:name]
      end
    end

    shared(:org_client_url){ "#{platform.server}/organizations/#{org_name}/clients" }
    shared(:user_url){ "#{platform.server}/users" }
    shared(:client_name) { unique_name("api-v1-client") }
    shared(:user_name) { unique_name("api-v1-user") }
    shared(:named_client_url) { "#{org_client_url}/#{client_name}" }
    shared(:named_user_url) { "#{user_url}/#{user_name}" }
    shared(:valid_pubkey) { @valid_pubkey ||= platform.gen_rsa_key("client-v1-test")[:public]}
    shared(:default_client_payload) {
      {
          "name" => client_name,
          "clientname" => client_name,
          "orgname" => org_name,
          "validator" => false
      }
    }
    shared(:default_user_payload) {
        {
          "username" => user_name,
          "email" => "#{user_name}@chef.io",
          "first_name" => user_name,
          "last_name" => user_name,
          "display_name" => user_name,
          "password" => "the panther strikes at midnight"
        }
    }
    before(:all) do
      # Note that a client is created by the server during org creation,
      # so we'll want to set our api version from the start.
      platform.use_max_server_api_version

      if Pedant.config[:org][:create_me]
        platform.create_org(org_name)
      end
    end

    after(:all) do
      if Pedant.config[:org][:create_me]
        platform.delete_org(org_name)
      end
      platform.reset_server_api_version
    end
    context "org creation", :organizations do
      let (:test_org_name) { unique_name("apiv1-org-create-test") }
      let (:org_payload)  {
          {
          "name" => test_org_name,
          "full_name" => test_org_name,
          "org_type" => "Business"
          }
      }
      after do
        platform.delete_org(test_org_name)
      end
      it "should have created a validator client with a default key, and provided the client key back to us" do
        r = post("#{platform.server}/organizations", superuser, :payload => org_payload)
        r.should look_like({ status: 201,
                             body: {
                                  "private_key" => privkey_regex,
                                  "clientname" => "#{test_org_name}-validator"
                             }})

        @clientname = parse(r)["clientname"]
        get("#{platform.server}/organizations/#{test_org_name}/clients/#{@clientname}/keys/default", superuser).should have_status_code 200
      end
    end

    skip "search results should not include client key data" do

    end

    # changed key behaviors are common across both users and clients.
    shared_context "actor read validation" do
      it "should not include public_key" do
        r = get(resource_url, superuser)
        r.should have_status_code 200
        parse(r)["public_key"].should eq nil
      end
    end

    shared_context "actor creation validation" do
      after(:each) do
        delete("#{named_resource_url}", superuser)
      end
      # Note: validation of valid public key values is enabled for api_v1+ in
      # client_util.rb, via clients/complete_endpoint_spec.rb, and so is not covered here.
      it "should allow create_key: true and give a proper valid key in response" do
        result = post(resource_url, superuser,
                      payload: create_payload.with('create_key', true))
        result.should look_like({status: 201,
                                 body_exact: { "uri" => named_resource_url,
                                                "chef_key" => { "uri" => "#{named_resource_url}/keys/default",
                                                                "name" => "default",
                                                                "private_key" => privkey_regex,
                                                                "public_key" => pubkey_regex,
                                                                "expiration_date" => "infinity" }  } })
        get("#{named_resource_url}/keys/default", superuser).should have_status_code 200
      end

      it "should allow allow public_key to be provided and respond with its location" do
        result = post(resource_url, superuser,
                      payload: create_payload.with('public_key', valid_pubkey))
        result.should look_like({status: 201,
                                 body_exact: { "uri" => named_resource_url,
                                               "chef_key" => { "uri" => "#{named_resource_url}/keys/default",
                                                               "name" => "default",
                                                               "public_key" => pubkey_regex,
                                                               "expiration_date" => "infinity" }  } })
      end

      it "should reply with an error if both create_key:true and public_key are specified", :validation do
        result = post(resource_url, superuser,
                      payload: create_payload.with('public_key', valid_pubkey).with("create_key", true))
        result.should have_status_code 400
      end

      it "should accept the public key if both create_key:false and public_key are specified" do
        result = post(resource_url, superuser,
                      payload: create_payload.with('public_key', valid_pubkey).with("create_key", false))
        result.should have_status_code 201

      end

      it "should reply with an error if private_key:true is specified for key generation", :validation do
        # TODO error message check?
        result = post(resource_url, superuser,
                      payload: create_payload.with('private_key', true))
        result.should have_status_code 400
      end

      it "when neither create_key nor public_key is specified, the operation should succeed and no default key is created" do
        result = post(resource_url, superuser, payload: create_payload)
        result.should have_status_code 201
        expect(parse(result).has_key?("chef_key")).to eq false
        get("#{named_resource_url}/keys/default", superuser).should have_status_code 404
      end

    end

    shared_context "actor update validation" do
      before :each do
        post("#{resource_url}", superuser, payload: create_payload).should have_status_code 201
      end
      after :each do
        delete("#{named_resource_url}", superuser).should have_status_code 200
      end

      it "should allow an update that doesn't include public_key or create_key" do
        put(named_resource_url, superuser, payload: create_payload).should have_status_code 200
      end

      it "should not allow create_key:true", :validation do
        put(named_resource_url, superuser, payload: create_payload.with('create_key', true)).should have_status_code 400
      end

      it "should not allow public_key to be provided", :validation do
        put(named_resource_url, superuser, payload: create_payload.with('public_key', valid_pubkey)).should have_status_code 400
      end
      it "should not allow private_key:true to be specified", :validation do
        put(named_resource_url, superuser, payload: create_payload.with('private_key', true)).should have_status_code 400
      end
    end

    context "users", :users do
      let(:create_payload) { default_user_payload }
      let(:resource_url) { user_url }
      let(:named_resource_url) { named_user_url }
      context "POST /users" do
        it_should_behave_like "actor creation validation"
      end
      context "PUT /users/:name" do
        it_should_behave_like "actor update validation"
      end
      context "GET /users/:name" do
        shared(:resource_url) { "#{user_url}/#{normal_user.name}" }
        it_behaves_like "actor read validation"
      end
      context "GET /organization/:org/users/:name" do
        # Use the default org member pedant provides us
        shared(:resource_url) { api_url("users/#{normal_user.name}") }
        it_behaves_like "actor read validation"
      end
    end

    context "clients", :clients do
      let(:create_payload) { default_client_payload }
      let(:resource_url) { org_client_url }
      let(:named_resource_url) { named_client_url }
      context "POST /organizations/:org/clients" do
        it_should_behave_like "actor creation validation"
      end
      context "PUT /organization/:org/clients/:name" do
        it_should_behave_like "actor update validation"
      end
      context "GET /organization/:org/clients/:name" do
        let (:resource_url) { "#{org_client_url}/#{org_name}-validator" }
        it_behaves_like "actor read validation"
      end
    end
  end
end

