require "pedant/rspec/common"

describe "Groups API Username Mapping for Multi-tenancy", :groups, :username_mapping do
  let(:org) { platform.test_org.name }
  let(:request_url) { api_url("groups") }
  let(:test_group_name) { "test-username-mapping-group-#{Time.now.to_i}" }
  let(:tenant_id) { "550e8400-e29b-41d4-a716-446655440000" }
  
  # Helper to add X-Ops-TenantId header to request
  def with_tenant_id_header(requestor, tenant_id)
    requestor.with_headers({ "x-ops-tenantid" => tenant_id })
  end

  before :each do
    # Clean up test group if it exists
    delete(api_url("groups/#{test_group_name}"), platform.admin_user) rescue nil
  end

  after :each do
    # Clean up test group
    delete(api_url("groups/#{test_group_name}"), platform.admin_user) rescue nil
  end

  context "POST /groups (create group)" do
    context "with X-Ops-TenantId header" do
      let(:legacy_username) { "test-user-legacy" }
      let(:mapped_username) { "#{legacy_username}__#{tenant_id}" }
      
      let(:request_body) {
        {
          "groupname" => test_group_name,
          "actors" => {
            "users" => [legacy_username],
            "clients" => ["test-client"],
            "groups" => []
          }
        }
      }

      it "appends tenant ID to usernames in request but not clients" do
        # Create group with legacy username in request
        post(request_url, with_tenant_id_header(platform.admin_user, tenant_id),
          payload: request_body).should look_like({
            status: 201,
            body: {
              "uri" => "#{request_url}/#{test_group_name}"
            }
          })

        # Verify the group was created with mapped username internally
        # When we GET, it should strip the tenant ID and return legacy username
        get(api_url("groups/#{test_group_name}"), platform.admin_user).should look_like({
          status: 200,
          body: {
            "groupname" => test_group_name,
            "name" => test_group_name,
            "orgname" => org,
            "users" => [legacy_username],  # Should be stripped back to legacy
            "clients" => ["test-client"],  # Clients unchanged
            "actors" => [legacy_username, "test-client"],
            "groups" => []
          }
        })
      end

      it "handles multiple users with tenant ID appending" do
        request_body = {
          "groupname" => test_group_name,
          "actors" => {
            "users" => ["user1", "user2", "user3"],
            "clients" => [],
            "groups" => []
          }
        }

        post(request_url, with_tenant_id_header(platform.admin_user, tenant_id),
          payload: request_body).should look_like({ status: 201 })

        response = get(api_url("groups/#{test_group_name}"), platform.admin_user)
        response.should look_like({ status: 200 })
        
        parsed = JSON.parse(response)
        parsed["users"].should match_array(["user1", "user2", "user3"])
      end

      it "handles empty users list" do
        request_body = {
          "groupname" => test_group_name,
          "actors" => {
            "users" => [],
            "clients" => ["test-client"],
            "groups" => []
          }
        }

        post(request_url, with_tenant_id_header(platform.admin_user, tenant_id),
          payload: request_body).should look_like({ status: 201 })

        get(api_url("groups/#{test_group_name}"), platform.admin_user).should look_like({
          status: 200,
          body: {
            "users" => [],
            "clients" => ["test-client"]
          }
        })
      end
    end

    context "without X-Ops-TenantId header" do
      let(:username) { "test-user-no-header" }
      
      let(:request_body) {
        {
          "groupname" => test_group_name,
          "actors" => {
            "users" => [username],
            "clients" => [],
            "groups" => []
          }
        }
      }

      it "creates group with usernames unchanged (no transformation)" do
        post(request_url, platform.admin_user,
          payload: request_body).should look_like({ status: 201 })

        get(api_url("groups/#{test_group_name}"), platform.admin_user).should look_like({
          status: 200,
          body: {
            "users" => [username],  # Username unchanged
            "groupname" => test_group_name
          }
        })
      end
    end
  end

  context "PUT /groups/<name> (update group)" do
    let(:group_url) { api_url("groups/#{test_group_name}") }

    before :each do
      # Create initial group with one user
      post(request_url, platform.admin_user,
        payload: {
          "groupname" => test_group_name,
          "actors" => {
            "users" => ["initial-user"],
            "clients" => [],
            "groups" => []
          }
        }).should look_like({ status: 201 })
    end

    context "with X-Ops-TenantId header" do
      it "appends tenant ID to new usernames in update" do
        update_body = {
          "groupname" => test_group_name,
          "actors" => {
            "users" => ["updated-user1", "updated-user2"],
            "clients" => ["test-client"],
            "groups" => []
          }
        }

        put(group_url, with_tenant_id_header(platform.admin_user, tenant_id),
          payload: update_body).should look_like({ status: 200 })

        # GET should strip tenant IDs from response
        get(group_url, platform.admin_user).should look_like({
          status: 200,
          body: {
            "users" => ["updated-user1", "updated-user2"],  # Stripped
            "clients" => ["test-client"],
            "groupname" => test_group_name
          }
        })
      end

      it "handles removing all users" do
        update_body = {
          "groupname" => test_group_name,
          "actors" => {
            "users" => [],
            "clients" => [],
            "groups" => []
          }
        }

        put(group_url, with_tenant_id_header(platform.admin_user, tenant_id),
          payload: update_body).should look_like({ status: 200 })

        get(group_url, platform.admin_user).should look_like({
          status: 200,
          body: {
            "users" => [],
            "groupname" => test_group_name
          }
        })
      end
    end

    context "without X-Ops-TenantId header" do
      it "updates group with usernames unchanged" do
        update_body = {
          "groupname" => test_group_name,
          "actors" => {
            "users" => ["new-user-no-header"],
            "clients" => [],
            "groups" => []
          }
        }

        put(group_url, platform.admin_user,
          payload: update_body).should look_like({ status: 200 })

        get(group_url, platform.admin_user).should look_like({
          status: 200,
          body: {
            "users" => ["new-user-no-header"],  # Unchanged
            "groupname" => test_group_name
          }
        })
      end
    end
  end

  context "GET /groups/<name> (retrieve group)" do
    context "when group contains usernames with tenant IDs" do
      let(:legacy_username) { "legacy-user" }
      let(:mapped_username) { "#{legacy_username}__#{tenant_id}" }

      before :each do
        # Create group with mapped username using tenant ID header
        post(request_url, with_tenant_id_header(platform.admin_user, tenant_id),
          payload: {
            "groupname" => test_group_name,
            "actors" => {
              "users" => [legacy_username],
              "clients" => [],
              "groups" => []
            }
          }).should look_like({ status: 201 })
      end

      it "strips tenant IDs from usernames in response" do
        response = get(api_url("groups/#{test_group_name}"), platform.admin_user)
        response.should look_like({ status: 200 })
        
        parsed = JSON.parse(response)
        parsed["users"].should == [legacy_username]  # Stripped
        parsed["actors"].should include(legacy_username)
      end

      it "strips tenant IDs from actors list as well" do
        response = get(api_url("groups/#{test_group_name}"), platform.admin_user)
        response.should look_like({ status: 200 })
        
        parsed = JSON.parse(response)
        # actors list should also have stripped usernames
        parsed["actors"].each do |actor|
          actor.should_not match(/__[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/)
        end
      end
    end
  end

  context "round-trip username transformation" do
    let(:original_username) { "roundtrip-user" }

    it "preserves original username through POST with header and GET" do
      # POST with tenant ID header (appends tenant ID internally)
      post(request_url, with_tenant_id_header(platform.admin_user, tenant_id),
        payload: {
          "groupname" => test_group_name,
          "actors" => {
            "users" => [original_username],
            "clients" => [],
            "groups" => []
          }
        }).should look_like({ status: 201 })

      # GET should return original username (stripped)
      response = get(api_url("groups/#{test_group_name}"), platform.admin_user)
      response.should look_like({ status: 200 })
      
      parsed = JSON.parse(response)
      parsed["users"].should == [original_username]
    end

    it "preserves username through multiple updates" do
      # Create with header
      post(request_url, with_tenant_id_header(platform.admin_user, tenant_id),
        payload: {
          "groupname" => test_group_name,
          "actors" => { "users" => ["user1"], "clients" => [], "groups" => [] }
        }).should look_like({ status: 201 })

      # Update with header (add another user)
      put(api_url("groups/#{test_group_name}"), with_tenant_id_header(platform.admin_user, tenant_id),
        payload: {
          "groupname" => test_group_name,
          "actors" => { "users" => ["user1", "user2"], "clients" => [], "groups" => [] }
        }).should look_like({ status: 200 })

      # GET should show both users without tenant IDs
      response = get(api_url("groups/#{test_group_name}"), platform.admin_user)
      parsed = JSON.parse(response)
      parsed["users"].should match_array(["user1", "user2"])
    end
  end

  context "edge cases" do
    context "username already contains double underscores" do
      let(:complex_username) { "user__with__underscores" }

      it "handles usernames with existing double underscores correctly" do
        post(request_url, with_tenant_id_header(platform.admin_user, tenant_id),
          payload: {
            "groupname" => test_group_name,
            "actors" => { "users" => [complex_username], "clients" => [], "groups" => [] }
          }).should look_like({ status: 201 })

        response = get(api_url("groups/#{test_group_name}"), platform.admin_user)
        parsed = JSON.parse(response)
        # Should preserve the original username structure
        parsed["users"].should == [complex_username]
      end
    end

    context "username contains UUID-like patterns" do
      let(:uuid_like_username) { "user-550e8400-e29b-41d4-a716-446655440000" }

      it "does not strip UUID patterns that are not tenant suffixes" do
        post(request_url, platform.admin_user,
          payload: {
            "groupname" => test_group_name,
            "actors" => { "users" => [uuid_like_username], "clients" => [], "groups" => [] }
          }).should look_like({ status: 201 })

        response = get(api_url("groups/#{test_group_name}"), platform.admin_user)
        parsed = JSON.parse(response)
        # Username should remain unchanged (UUID not preceded by __)
        parsed["users"].should == [uuid_like_username]
      end
    end
  end
end
