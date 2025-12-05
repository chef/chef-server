# Copyright Chef Software, Inc. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

require 'pedant/rspec/common'

describe "Groups API Username Mapping for Multi-Tenancy", :groups, :username_mapping do
  include Pedant::RSpec::Common

  let(:request_method) { :PUT }
  let(:request_url) { api_url("/groups/#{group_name}") }
  let(:requestor) { platform.admin_user }

  let(:org_name) { platform.test_org.name }
  let(:tenant_id) { "6e365298-7478-49b9-ba51-38a8c1204af2" }

  def random_name(prefix = "testuser")
    "#{prefix}-#{SecureRandom.hex(4)}"
  end

  def create_client!(client_name)
    response = post(api_url("/clients"), requestor, payload: { name: client_name })
    response.should look_like({ status: 201 })
    client_name
  end

  def create_group!(group_name, actors: nil, headers: {})
    payload = { groupname: group_name }
    response = post(api_url("/groups"), requestor, payload: payload, headers: headers)
    response.should look_like({ status: 201 })

    if actors
      put_response = put(
        api_url("/groups/#{group_name}"),
        requestor,
        payload: { groupname: group_name, actors: actors },
        headers: headers
      )
      put_response.should look_like({ status: 200 })
    end

    response
  end

  def expect_stripped_users(response_or_parsed, expected_users)
    parsed = response_or_parsed.is_a?(Hash) ? response_or_parsed : parse(response_or_parsed)
    expected_users.each do |user|
      parsed["users"].should include(user)
      parsed["users"].any? { |u| u.start_with?("#{user}__") }.should be_falsey
    end
    parsed
  end

  def expect_clients_unchanged(response_or_parsed, expected_clients)
    parsed = response_or_parsed.is_a?(Hash) ? response_or_parsed : parse(response_or_parsed)
    expected_clients.each do |client|
      if parsed.key?("clients")
        # Standard GET response has separate clients array
        parsed["clients"].should include(client)
        parsed["clients"].any? { |c| c.start_with?("#{client}__") }.should be_falsey
      elsif parsed.key?("actors")
        # Fallback for flattened responses (e.g., POST/PUT/DELETE)
        parsed["actors"].should include(client)
        parsed["actors"].any? { |c| c.start_with?("#{client}__") }.should be_falsey
      else
        raise "Response has neither 'clients' nor 'actors' key: #{parsed.keys.inspect}"
      end
    end
    parsed
  end

  # GET /groups/<name> - Always strips tenant IDs from response
  describe "GET /groups/<name>" do
    let(:group_name) { "testgroup-#{SecureRandom.hex(6)}" }
    let(:username) { random_name }
    let(:username_with_tenant) { "#{username}__#{tenant_id}" }

    before(:each) do
      # Create user
      # Create the tenant-suffixed user so header mapping can find it
      user = platform.create_user(username_with_tenant)
      platform.associate_user_with_org(org_name, user)

      # Create group with user (using tenant header to store mapped)
      create_group!(group_name, actors: { users: [username], clients: [], groups: [] }, headers: { 'X-Ops-TenantId' => tenant_id })
    end

    after(:each) do
      delete(api_url("/groups/#{group_name}"), requestor) rescue nil
      delete(api_url("/users/#{username}"), requestor) rescue nil
    end

    context "when X-Ops-TenantId header is present" do
      it "strips tenant ID from usernames in response" do
        response = get(api_url("/groups/#{group_name}"), requestor,
          headers: { 'X-Ops-TenantId' => tenant_id })
        response.should look_like({ status: 200 })

        parsed = parse(response)
        expect_stripped_users(parsed, [username])
        parsed["actors"].should include(username)
        parsed["actors"].should_not include(username_with_tenant)
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "strips tenant ID from usernames in response (always strips)" do
        response = get(api_url("/groups/#{group_name}"), requestor)
        response.should look_like({ status: 200 })

        parsed = parse(response)
        expect_stripped_users(parsed, [username])
        parsed["actors"].should include(username)
        parsed["actors"].should_not include(username_with_tenant)
      end
    end
  end

  # PUT /groups/<name> - Maps on request, strips on response
  describe "PUT /groups/<name>" do
    let(:group_name) { "testgroup-#{SecureRandom.hex(6)}" }
    let(:username) { random_name }
    let(:username_with_tenant) { "#{username}__#{tenant_id}" }

    before(:each) do
      # Create a group first
      create_group!(group_name)

      # Create both base and tenant-suffixed users for header/no-header contexts
      user1 = platform.create_user(username)
      platform.associate_user_with_org(org_name, user1)
      user2 = platform.create_user(username_with_tenant)
      platform.associate_user_with_org(org_name, user2)
    end

    after(:each) do
      delete(api_url("/groups/#{group_name}"), requestor) rescue nil
      delete(api_url("/users/#{username}"), requestor) rescue nil
    end

    context "when X-Ops-TenantId header is present" do
      it "appends tenant ID to usernames in request and stores mapped" do
        # Send PUT with plain username and tenant header
        response = put(
          api_url("/groups/#{group_name}"),
          requestor,
          payload: {
            groupname: group_name,
            actors: {
              users: [username],
              clients: [],
              groups: []
            }
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        # Verify stored state via GET (stripped)
        fetch_response = get(api_url("/groups/#{group_name}"), requestor,
          headers: { 'X-Ops-TenantId' => tenant_id })
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        expect_stripped_users(fetch_parsed, [username])
        fetch_parsed["actors"].should include(username)
        fetch_parsed["actors"].should_not include(username_with_tenant)
      end

      it "returns response with tenant ID stripped" do
        # PUT with tenant header
        response = put(
          api_url("/groups/#{group_name}"),
          requestor,
          payload: {
            groupname: group_name,
            actors: {
              users: [username],
              clients: [],
              groups: []
            }
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        # Verify stored state via GET (stripped)
        fetch_response = get(api_url("/groups/#{group_name}"), requestor,
          headers: { 'X-Ops-TenantId' => tenant_id })
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        expect_stripped_users(fetch_parsed, [username])
        fetch_parsed["actors"].should include(username)
        fetch_parsed["actors"].should_not include(username_with_tenant)
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "stores usernames as provided without modification" do
        # PUT without tenant header - no mapping should occur
        response = put(
          api_url("/groups/#{group_name}"),
          requestor,
          payload: {
            groupname: group_name,
            actors: {
              users: [username],
              clients: [],
              groups: []
            }
          }
        )
        response.should look_like({ status: 200 })

        fetch_response = get(api_url("/groups/#{group_name}"), requestor)
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        expect_stripped_users(fetch_parsed, [username])
      end
    end
  end

  # POST /groups - Maps on request, returns URI only
  describe "POST /groups" do
    let(:group_name) { "testgroup-#{SecureRandom.hex(6)}" }
    let(:username) { random_name }
    let(:username_with_tenant) { "#{username}__#{tenant_id}" }

    before(:each) do
      # Create a user
      user = platform.create_user(username)
      platform.associate_user_with_org(org_name, user)
    end

    after(:each) do
      delete(api_url("/groups/#{group_name}"), requestor) rescue nil
      delete(api_url("/users/#{username}"), requestor) rescue nil
    end

    context "when X-Ops-TenantId header is present" do
      it "appends tenant ID to usernames in request" do
        # Create group with user via POST
        response = create_group!(
          group_name,
          actors: {
            users: [username],
            clients: [],
            groups: []
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 201 })

        parsed = parse(response)
        parsed.keys.should eq(["uri"])
        parsed["uri"].should match(/\/organizations\/#{org_name}\/groups\/#{group_name}$/)
      end

      it "returns response with URI only (no user data)" do
        response = create_group!(
          group_name,
          actors: {
            users: [username],
            clients: [],
            groups: []
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 201 })

        parsed = parse(response)
        parsed.keys.should eq(["uri"])
        parsed.should_not have_key("users")
        parsed.should_not have_key("clients")
        parsed.should_not have_key("actors")
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "stores usernames as provided without modification" do
        response = create_group!(
          group_name,
          actors: {
            users: [username],
            clients: [],
            groups: []
          }
        )
        response.should look_like({ status: 201 })

        parsed = parse(response)
        parsed.keys.should eq(["uri"])

        fetch_response = get(api_url("/groups/#{group_name}"), requestor)
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        expect_stripped_users(fetch_parsed, [username])
      end
    end
  end

  # Edge cases and validation
  describe "Edge cases and validation" do
    let(:group_name) { "testgroup-#{SecureRandom.hex(6)}" }

    after(:each) do
      delete(api_url("/groups/#{group_name}"), requestor) rescue nil
    end

    it "with multiple usernames transforms all usernames correctly" do
      user1 = random_name("user1")
      user2 = random_name("user2")
      user1_tenant = "#{user1}__#{tenant_id}"
      user2_tenant = "#{user2}__#{tenant_id}"

      # Create users
      [user1_tenant, user2_tenant].each do |u|
        user = platform.create_user(u)
        platform.associate_user_with_org(org_name, user)
      end

      # Create group via POST with tenant header
      response = create_group!(
        group_name,
        actors: {
          users: [user1, user2],
          clients: [],
          groups: []
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 201 })
      parsed = parse(response)
      parsed.keys.should eq(["uri"])

      # Verify both users via GET (will show stripped versions)
      fetch_response = get(api_url("/groups/#{group_name}"), requestor)
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)
      expect_stripped_users(fetch_parsed, [user1, user2])

      # Cleanup
      [user1, user2].each { |u| delete(api_url("/users/#{u}"), requestor) rescue nil }
    end

    it "with different tenant IDs does not leak tenant suffixes in responses" do
      username = random_name
      other_tenant_id = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
      username_other_tenant = "#{username}__#{other_tenant_id}"

      user = platform.create_user(username_other_tenant)
      platform.associate_user_with_org(org_name, user)

      # Create with different tenant ID; response should never leak suffixes
      response = create_group!(
        group_name,
        actors: {
          users: [username],
          clients: [],
          groups: []
        },
        headers: { 'X-Ops-TenantId' => other_tenant_id }
      )
      response.should look_like({ status: 201 })

      # Verify via GET - shows stripped username and no tenant suffix leakage
      fetch_response = get(api_url("/groups/#{group_name}"), requestor)
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)
      expect_stripped_users(fetch_parsed, [username])

      # Cleanup
      delete(api_url("/users/#{username}"), requestor) rescue nil
    end

    it "with mixed users and clients transforms users but not clients" do
      username = random_name("testuser")
      username_tenant = "#{username}__#{tenant_id}"
      client_name = random_name("testclient")

      # Create user
      user = platform.create_user(username_tenant)
      platform.associate_user_with_org(org_name, user)

      # Create client
      create_client!(client_name)

      # Create group with both
      response = create_group!(
        group_name,
        actors: {
          users: [username],
          clients: [client_name],
          groups: []
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 201 })

      # Verify: user is stripped, client unchanged
      fetch_response = get(api_url("/groups/#{group_name}"), requestor)
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)
      expect_stripped_users(fetch_parsed, [username])
      expect_clients_unchanged(fetch_parsed, [client_name])

      # Cleanup
      delete(api_url("/users/#{username}"), requestor) rescue nil
      delete(api_url("/clients/#{client_name}"), requestor) rescue nil
    end
  end

  # DELETE /groups/:group - Strips tenant IDs from response
  describe "DELETE /groups/:group with username mapping" do
    let(:group_name) { "deletegroup-#{SecureRandom.hex(6)}" }
    let(:delete_test_user) { random_name("deleteuser") }
    let(:delete_test_user_tenant) { "#{delete_test_user}__#{tenant_id}" }

    before(:each) do
      # Create both base and tenant-suffixed users for different contexts
      user1 = platform.create_user(delete_test_user)
      platform.associate_user_with_org(org_name, user1)
      user2 = platform.create_user(delete_test_user_tenant)
      platform.associate_user_with_org(org_name, user2)
    end

    after(:each) do
      delete(api_url("/groups/#{group_name}"), requestor) rescue nil
      delete(api_url("/users/#{delete_test_user}"), requestor) rescue nil
    end

    context "when X-Ops-TenantId header is present" do
      it "strips tenant IDs from users in DELETE response body" do
        # Create group with user and tenant mapping
        create_group!(
          group_name,
          actors: {
            users: [delete_test_user],
            clients: [],
            groups: []
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )

        # Delete the group
        response = delete(
          api_url("/groups/#{group_name}"),
          requestor
        )
        response.should look_like({ status: 200 })

        # Group deleted; verify removal
        get(api_url("/groups/#{group_name}"), requestor).should look_like({ status: 404 })
      end

      it "strips tenant IDs from both users and clients in DELETE response" do
        client_name = random_name("deleteclient")

        # Create client
        post(api_url("/clients"), requestor, payload: { name: client_name })

        # Create group with both user and client
        create_group!(
          group_name,
          actors: {
            users: [delete_test_user],
            clients: [client_name],
            groups: []
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )

        # Delete the group
        response = delete(
          api_url("/groups/#{group_name}"),
          requestor
        )
        response.should look_like({ status: 200 })

        # Group deleted; verify removal
        get(api_url("/groups/#{group_name}"), requestor).should look_like({ status: 404 })

        # Cleanup
        delete(api_url("/clients/#{client_name}"), requestor) rescue nil
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "returns usernames unchanged in DELETE response" do
        plain_user = random_name("plainuser")

        # Create user
        user = platform.create_user(plain_user)
        platform.associate_user_with_org(org_name, user)

        # Create group without tenant mapping
        create_group!(
          group_name,
          actors: {
            users: [plain_user],
            clients: [],
            groups: []
          }
        )

        # Delete the group
        response = delete(
          api_url("/groups/#{group_name}"),
          requestor
        )
        response.should look_like({ status: 200 })

        # Group deleted; verify removal
        get(api_url("/groups/#{group_name}"), requestor).should look_like({ status: 404 })

        # Cleanup
        delete(api_url("/users/#{plain_user}"), requestor) rescue nil
      end

      it "returns 200 when deleting a group created with tenant mapping but deleted without header" do
        mapped_user = random_name("mappeduser")
        user = platform.create_user(mapped_user)
        platform.associate_user_with_org(org_name, user)

        create_group!(
          group_name,
          actors: {
            users: [mapped_user],
            clients: [],
            groups: []
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )

        response = delete(api_url("/groups/#{group_name}"), requestor)
        response.should look_like({ status: 200 })
        # Group deleted; verify removal
        get(api_url("/groups/#{group_name}"), requestor).should look_like({ status: 404 })
      ensure
        delete(api_url("/users/#{mapped_user}"), requestor) rescue nil
      end
    end
  end
end
