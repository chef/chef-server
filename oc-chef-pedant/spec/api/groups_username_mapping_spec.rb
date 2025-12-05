require "pedant/rspec/common"

describe "Groups API Username Mapping for Multi-Tenancy", :groups do
  let(:org) { platform.test_org.name }
  let(:request_url) { api_url("groups") }
  let(:test_group) { "username-mapping-test-#{rand(1000000)}" }
  let(:group_url) { api_url("groups/#{test_group}") }

  # Test data
  let(:tenant_id) { "test-tenant-12345" }
  let(:username) { "testuser" }
  let(:username_with_tenant) { "testuser__test-tenant-12345" }

  before :each do
    # Create test group
    post(request_url, platform.admin_user,
      payload: { "groupname" => test_group }).should look_like({ status: 201 })
  end

  after :each do
    # Clean up test group
    delete(group_url, platform.admin_user)
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
      let(:usernames) { ["user1", "user2", "user3"] }
      let(:usernames_with_tenant) { usernames.map { |u| "#{u}__#{tenant_id}" } }

      it "transforms all usernames correctly" do
        response = post(request_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => edge_test_group,
            "actors" => {
              "users" => usernames,
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        # Verify stored with tenant suffixes
        fetch_response = get(edge_group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed = parse(fetch_response)
        usernames_with_tenant.each do |un|
          parsed["users"].should include(un)
          parsed["actors"].should include(un)
        end
      end
    end

    context "with different tenant IDs" do
      let(:tenant_id_1) { "tenant-one" }
      let(:tenant_id_2) { "tenant-two" }
      let(:request_headers_1) { { "x-ops-tenantid" => tenant_id_1 } }
      let(:request_headers_2) { { "x-ops-tenantid" => tenant_id_2 } }

      it "uses the correct tenant ID for transformations" do
        # Create with tenant 1
        response = post(request_url, platform.admin_user,
          headers: request_headers_1,
          payload: {
            "groupname" => edge_test_group,
            "actors" => {
              "users" => [username],
              "clients" => [],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        # Verify stored with tenant-one suffix (no header)
        fetch_response = get(edge_group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed = parse(fetch_response)
        parsed["users"].should include("#{username}__#{tenant_id_1}")

        # Fetch with tenant 2 header - should NOT strip since tenant IDs don't match
        fetch_with_tenant_2 = get(edge_group_url, platform.admin_user,
          headers: request_headers_2)
        fetch_with_tenant_2.should look_like({ status: 200 })
        
        parsed_2 = parse(fetch_with_tenant_2)
        # Should still have tenant-one suffix since it won't match tenant-two
        parsed_2["users"].should include("#{username}__#{tenant_id_1}")
      end
    end

    context "with mixed users and clients" do
      let(:request_headers) { { "x-ops-tenantid" => tenant_id } }
      let(:client_name) { "test-client" }

      it "transforms users but not clients" do
        response = post(request_url, platform.admin_user,
          headers: request_headers,
          payload: {
            "groupname" => edge_test_group,
            "actors" => {
              "users" => [username],
              "clients" => [client_name],
              "groups" => []
            }
          })
        response.should look_like({ status: 201 })

        # Verify: users get tenant suffix, clients don't
        fetch_response = get(edge_group_url, platform.admin_user)
        fetch_response.should look_like({ status: 200 })
        
        parsed = parse(fetch_response)
        parsed["users"].should include(username_with_tenant)
        parsed["clients"].should include(client_name)
        # clients should NOT have tenant suffix
        parsed["clients"].should_not include("#{client_name}__#{tenant_id}")
      end
    end
  end
end
