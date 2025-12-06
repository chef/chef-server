require "pedant/rspec/common"

describe "Groups API Username Mapping for Multi-Tenancy", :groups do
  let(:org) { platform.test_org.name }
  let(:request_url) { api_url("groups") }
  let(:test_group) { "username-mapping-test-#{rand(1000000)}" }
  let(:group_url) { api_url("groups/#{test_group}") }

  # Test data
  let(:tenant_id) { "test-tenant-12345" }
  let(:username) { "testuser-#{SecureRandom.hex(4)}" }
  let(:username_with_tenant) { "#{username}__#{tenant_id}" }
  let(:users_url) { "#{platform.server}/users" }

  before :each do
    # Create test user with tenant suffix
    post(users_url, platform.superuser,
      payload: {
        "username" => username_with_tenant,
        "email" => "#{username_with_tenant}@test.com",
        "first_name" => username,
        "last_name" => "Test",
        "display_name" => username_with_tenant,
        "password" => "testpass123"
      }).should look_like({ status: 201 })
    
    # Associate user with org
    post("#{platform.server}/organizations/#{org}/users", platform.superuser,
      payload: { "username" => username_with_tenant }).should look_like({ status: 201 })
    
    # Create test group
    post(request_url, platform.admin_user,
      payload: { "groupname" => test_group }).should look_like({ status: 201 })
  end

  after :each do
    # Clean up test group
    delete(group_url, platform.admin_user) rescue nil
    
    # Clean up test user
    delete_user(username_with_tenant, platform.superuser) rescue nil
  end

  describe "GET /groups/<name>" do
    context "when X-Ops-TenantId header is present" do
      let(:request_headers) { { "x-ops-tenantid" => tenant_id } }

      before :each do
        # Add user with tenant suffix to group
        put(group_url, platform.admin_user,
          payload: {
            "groupname" => test_group,
            "actors" => {
              "users" => [username_with_tenant],
              "clients" => [],
              "groups" => []
            }
          }).should look_like({ status: 200 })
      end

      it "strips tenant ID from usernames in response" do
        response = get(group_url, platform.admin_user, headers: request_headers)
        response.should look_like({ status: 200 })
        
        parsed = parse(response)
        parsed["users"].should include(username)
        parsed["users"].should_not include(username_with_tenant)
        parsed["actors"].should include(username)
        parsed["actors"].should_not include(username_with_tenant)
      end
    end

    context "when X-Ops-TenantId header is absent" do
      before :each do
        # Add user with tenant suffix to group
        put(group_url, platform.admin_user,
          payload: {
            "groupname" => test_group,
            "actors" => {
              "users" => [username_with_tenant],
              "clients" => [],
              "groups" => []
            }
          }).should look_like({ status: 200 })
      end

      it "returns usernames with tenant ID unchanged" do
        response = get(group_url, platform.admin_user)
        response.should look_like({ status: 200 })
        
        parsed = parse(response)
        parsed["users"].should include(username_with_tenant)
        parsed["actors"].should include(username_with_tenant)
      end
    end
  end

  describe "PUT /groups/<name>" do
    context "when X-Ops-TenantId header is present" do
      let(:request_headers) { { "x-ops-tenantid" => tenant_id } }

      it "appends tenant ID to usernames in request" do
        # Update with plain username (no tenant suffix)
        put(group_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => test_group,
            "actors" => {
              "users" => [username],
              "clients" => [],
              "groups" => []
            }
          }).should look_like({ status: 200 })

        # Verify by fetching without header - should show tenant-suffixed username
        response = get(group_url, platform.admin_user)
        response.should look_like({ status: 200 })
        
        parsed = parse(response)
        parsed["users"].should include(username_with_tenant)
        parsed["users"].should_not include(username)
        parsed["actors"].should include(username_with_tenant)
        parsed["actors"].should_not include(username)
      end

      it "returns response with tenant ID stripped" do
        response = put(group_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => test_group,
            "actors" => {
              "users" => [username],
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 200 })

        parsed = parse(response)
        parsed["users"].should include(username)
        parsed["users"].should_not include(username_with_tenant)
        parsed["actors"].should include(username)
        parsed["actors"].should_not include(username_with_tenant)
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "stores usernames as provided without modification" do
        put(group_url, platform.admin_user,
          payload: {
            "groupname" => test_group,
            "actors" => {
              "users" => [username_with_tenant],
              "clients" => [],
              "groups" => []
            }
          }).should look_like({ status: 200 })

        # Verify by fetching
        response = get(group_url, platform.admin_user)
        response.should look_like({ status: 200 })
        
        parsed = parse(response)
        parsed["users"].should include(username_with_tenant)
        parsed["actors"].should include(username_with_tenant)
      end
    end
  end

  describe "POST /groups" do
    let(:new_test_group) { "username-mapping-new-#{rand(1000000)}" }
    let(:new_group_url) { api_url("groups/#{new_test_group}") }

    after :each do
      delete(new_group_url, platform.admin_user) rescue nil
    end

    context "when X-Ops-TenantId header is present" do
      let(:request_headers) { { "x-ops-tenantid" => tenant_id } }

      it "appends tenant ID to usernames in request" do
        response = post(request_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => new_test_group,
            "actors" => {
              "users" => [username],
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        # Verify by fetching without header
        fetch_response = get(new_group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed = parse(fetch_response)
        parsed["users"].should include(username_with_tenant)
        parsed["actors"].should include(username_with_tenant)
      end

      it "returns response with tenant ID stripped" do
        response = post(request_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => new_test_group,
            "actors" => {
              "users" => [username],
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        parsed = parse(response)
        parsed["users"].should include(username)
        parsed["users"].should_not include(username_with_tenant)
        parsed["actors"].should include(username)
        parsed["actors"].should_not include(username_with_tenant)
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "stores usernames as provided without modification" do
        response = post(request_url, platform.admin_user,
          payload: {
            "groupname" => new_test_group,
            "actors" => {
              "users" => [username_with_tenant],
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        # Verify by fetching
        fetch_response = get(new_group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed = parse(fetch_response)
        parsed["users"].should include(username_with_tenant)
        parsed["actors"].should include(username_with_tenant)
      end
    end
  end

  describe "Edge cases and validation" do
    let(:edge_test_group) { "username-mapping-edge-#{rand(1000000)}" }
    let(:edge_group_url) { api_url("groups/#{edge_test_group}") }

    after :each do
      delete(edge_group_url, platform.admin_user) rescue nil
    end

    context "with multiple usernames" do
      let(:request_headers) { { "x-ops-tenantid" => tenant_id } }

      it "transforms all usernames correctly" do
        # Use the user we already created plus existing platform users
        response = post(request_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => edge_test_group,
            "actors" => {
              "users" => [username, platform.non_admin_user.name],
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        # Verify in response: our user stripped, platform user untouched
        parsed = parse(response)
        parsed["users"].should include(username)
        parsed["users"].should include(platform.non_admin_user.name)
        parsed["users"].should_not include(username_with_tenant)

        # Verify stored: our user has tenant suffix, platform user unchanged
        fetch_response = get(edge_group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed = parse(fetch_response)
        parsed["users"].should include(username_with_tenant)
        parsed["users"].should include(platform.non_admin_user.name)
      end
    end

    context "with different tenant IDs" do
      let(:tenant_id_2) { "tenant-two" }
      let(:request_headers_2) { { "x-ops-tenantid" => tenant_id_2 } }

      it "uses the correct tenant ID for transformations" do
        # Update group with different tenant ID
        response = put(group_url, platform.admin_user,
          headers: request_headers_2,
          payload: {
            "groupname" => test_group,
            "actors" => {
              "users" => [username],
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 200 })

        # Response should strip with tenant_id_2
        parsed = parse(response)
        parsed["users"].should include(username)
        parsed["users"].should_not include("#{username}__#{tenant_id_2}")

        # But storage should have tenant_id_2 suffix
        fetch_response = get(group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed_fetch = parse(fetch_response)
        # Note: The user was created with tenant_id, but we're requesting with tenant_id_2
        # So it will store as username__tenant_id_2, but that user doesn't exist
        # This is actually testing error handling - the group will have the username with wrong tenant
        parsed_fetch["users"].should include("#{username}__#{tenant_id_2}")
      end
    end

    context "with mixed users and clients" do
      let(:request_headers) { { "x-ops-tenantid" => tenant_id } }
      let(:test_client) { "test-client-#{SecureRandom.hex(4)}" }

      before :each do
        # Create test client
        post(api_url("clients"), platform.admin_user,
          payload: {
            "name" => test_client,
            "clientname" => test_client,
            "create_key" => true
          }).should look_like({ status: 201 })
      end

      after :each do
        # Cleanup client
        delete(api_url("clients/#{test_client}"), platform.admin_user) rescue nil
      end

      it "transforms users but not clients" do
        response = post(request_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => edge_test_group,
            "actors" => {
              "users" => [username],
              "clients" => [test_client],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        # Verify in response: user stripped, client unchanged
        parsed = parse(response)
        parsed["users"].should include(username)
        parsed["users"].should_not include(username_with_tenant)
        parsed["clients"].should include(test_client)

        # Verify stored: user has tenant suffix, client unchanged
        fetch_response = get(edge_group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed = parse(fetch_response)
        parsed["users"].should include(username_with_tenant)
        parsed["clients"].should include(test_client)
        # Both should be in actors
        parsed["actors"].should include(username_with_tenant)
        parsed["actors"].should include(test_client)
        # Client should NOT have tenant suffix
        parsed["clients"].should_not include("#{test_client}__#{tenant_id}")
      end
    end
  end
end
