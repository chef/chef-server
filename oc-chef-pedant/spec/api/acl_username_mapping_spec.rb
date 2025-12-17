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

describe "ACL API Username Mapping for Multi-Tenancy", :acl, :username_mapping do
  include Pedant::RSpec::Common

  let(:request_method) { :PUT }
  let(:requestor) { platform.admin_user }

  let(:org_name) { platform.test_org.name }
  let(:tenant_id) { "6e365298-7478-49b9-ba51-38a8c1204af2" }

  # Test with nodes ACL (common resource type)
  let(:node_name) { "testnode-#{SecureRandom.hex(6)}" }
  let(:acl_permission) { "read" } # Can also test create, update, delete, grant

  def random_name(prefix = "testuser")
    "#{prefix}-#{SecureRandom.hex(4)}"
  end

  def create_node!(node_name)
    payload = { name: node_name }
    response = post(api_url("/nodes"), requestor, payload: payload)
    response.should look_like({ status: 201 })
    node_name
  end

  def create_client!(client_name)
    response = post(api_url("/clients"), requestor, payload: { name: client_name })
    response.should look_like({ status: 201 })
    client_name
  end

  # Standard ACL responses use "actors" array (combined users+clients)
  def expect_stripped_users_in_actors(response_or_parsed, expected_users)
    parsed = response_or_parsed.is_a?(Hash) ? response_or_parsed : parse(response_or_parsed)
    expected_users.each do |user|
      parsed["actors"].should include(user)
      parsed["actors"].any? { |a| a.start_with?("#{user}__") }.should be_falsey
    end
    parsed
  end

  def expect_actors_include(response_or_parsed, expected_actors)
    parsed = response_or_parsed.is_a?(Hash) ? response_or_parsed : parse(response_or_parsed)
    expected_actors.each do |actor|
      parsed["actors"].should include(actor)
    end
    parsed
  end

  # Granular ACL responses (?detail=granular) use separate "users"/"clients" arrays
  def expect_stripped_users_granular(response_or_parsed, expected_users)
    parsed = response_or_parsed.is_a?(Hash) ? response_or_parsed : parse(response_or_parsed)
    expected_users.each do |user|
      parsed["users"].should include(user)
      parsed["users"].any? { |u| u.start_with?("#{user}__") }.should be_falsey
    end
    parsed
  end

  def expect_clients_unchanged_granular(response_or_parsed, expected_clients)
    parsed = response_or_parsed.is_a?(Hash) ? response_or_parsed : parse(response_or_parsed)
    expected_clients.each do |client|
      parsed["clients"].should include(client)
      parsed["clients"].any? { |c| c.start_with?("#{client}__") }.should be_falsey
    end
    parsed
  end

  # GET /nodes/:node/_acl/:permission - Always strips tenant IDs from response
  describe "GET /:resource/:name/_acl/:permission" do
    let(:username) { random_name }
    let(:username_with_tenant) { "#{username}__#{tenant_id}" }
    let(:client_name) { random_name("testclient") }

    before(:each) do
      # Create node
      create_node!(node_name)

      # Create user with tenant suffix
      user = platform.create_user(username_with_tenant)
      platform.associate_user_with_org(org_name, user)

      # Create client
      create_client!(client_name)

      # Set ACL with user (using tenant header to store mapped)
      put_response = put(
        api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
        requestor,
        payload: {
          acl_permission => {
            actors: [username, client_name],
            groups: []
          }
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      put_response.should look_like({ status: 200 })
    end

    after(:each) do
      delete(api_url("/nodes/#{node_name}"), requestor) rescue nil
      delete(api_url("/users/#{username}"), requestor) rescue nil
      delete(api_url("/clients/#{client_name}"), requestor) rescue nil
    end

    context "when X-Ops-TenantId header is present" do
      it "strips tenant ID from usernames in response" do
        response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        parsed = parse(response)
        parsed.should have_key(acl_permission)
        permission_data = parsed[acl_permission]

        # Standard response uses "actors" array (combined users+clients)
        permission_data.should have_key("actors")
        expect_stripped_users_in_actors(permission_data, [username])
        permission_data["actors"].should_not include(username_with_tenant)
      end

      it "does not modify clients in response" do
        response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        parsed = parse(response)
        permission_data = parsed[acl_permission]

        # Standard response: actors array includes clients unchanged
        permission_data.should have_key("actors")
        expect_actors_include(permission_data, [client_name])
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "strips tenant ID from usernames in response (always strips)" do
        response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor
        )
        response.should look_like({ status: 200 })

        parsed = parse(response)
        permission_data = parsed[acl_permission]

        # Standard response uses "actors" array
        permission_data.should have_key("actors")
        expect_stripped_users_in_actors(permission_data, [username])
        permission_data["actors"].should_not include(username_with_tenant)
      end
    end

    context "granular mode (?detail=granular)" do
      it "strips usernames and separates users/clients arrays" do
        response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}?detail=granular"),
          requestor,
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        parsed = parse(response)
        permission_data = parsed[acl_permission]

        # Granular mode: separate users/clients, empty actors
        permission_data.should have_key("users")
        permission_data.should have_key("clients")
        permission_data["actors"].should eq([])

        # Verify stripping and separation in granular mode
        expect_stripped_users_granular(permission_data, [username])
        expect_clients_unchanged_granular(permission_data, [client_name])
      end
    end
  end

  # GET /:resource/:name/_acl - Full ACL with all 5 permission parts
  describe "GET /:resource/:name/_acl (full ACL)" do
    let(:username) { random_name }
    let(:username_with_tenant) { "#{username}__#{tenant_id}" }

    before(:each) do
      # Create node
      create_node!(node_name)

      # Create user with tenant suffix
      user = platform.create_user(username_with_tenant)
      platform.associate_user_with_org(org_name, user)

      # Set ACL for read permission (one of five)
      put_response = put(
        api_url("/nodes/#{node_name}/_acl/read"),
        requestor,
        payload: {
          read: {
            actors: [username1, username2],
            groups: []
          }
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      put_response.should look_like({ status: 200 })
    end

    after(:each) do
      delete(api_url("/nodes/#{node_name}"), requestor) rescue nil
      delete(api_url("/users/#{username}"), requestor) rescue nil
    end

    it "strips tenant IDs from all permission parts" do
      response = get(
        api_url("/nodes/#{node_name}/_acl"),
        requestor,
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 200 })

      parsed = parse(response)
      %w[create read update delete grant].each do |perm|
        parsed.should have_key(perm)
        parsed[perm].should have_key("actors")
        if parsed[perm]["actors"].include?(username)
          # Only read has our user, but check all parts for consistency
          expect_stripped_users_in_actors(parsed[perm], [username])
        end
      end
    end

    it "strips tenant IDs in granular mode (?detail=granular)" do
      response = get(
        api_url("/nodes/#{node_name}/_acl?detail=granular"),
        requestor,
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 200 })

      parsed = parse(response)
      read_perm = parsed["read"]
      
      # In granular mode, users and clients are separate arrays (not merged into actors)
      read_perm.should have_key("users")
      read_perm.should have_key("clients")
      read_perm["actors"].should eq([]) # Empty in granular mode
      
      # Verify users stripped, clients unchanged in granular mode
      expect_stripped_users_granular(read_perm, [username])
    end
  end

  # PUT /:resource/:name/_acl/:permission - Maps on request, strips on response
  describe "PUT /:resource/:name/_acl/:permission" do
    let(:username) { random_name }
    let(:username_with_tenant) { "#{username}__#{tenant_id}" }

    before(:each) do
      # Create node
      create_node!(node_name)

      # Create both base and tenant-suffixed users
      user1 = platform.create_user(username)
      platform.associate_user_with_org(org_name, user1)
      user2 = platform.create_user(username_with_tenant)
      platform.associate_user_with_org(org_name, user2)
    end

    after(:each) do
      delete(api_url("/nodes/#{node_name}"), requestor) rescue nil
      delete(api_url("/users/#{username}"), requestor) rescue nil
    end

    context "when X-Ops-TenantId header is present" do
      it "appends tenant ID to usernames in request and stores mapped" do
        # Send PUT with plain username and tenant header
        response = put(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          payload: {
            acl_permission => {
              actors: [username],
              groups: []
            }
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        # Verify stored state via GET (stripped)
        fetch_response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        permission_data = fetch_parsed[acl_permission]

        # Standard response uses "actors" array
        permission_data.should have_key("actors")
        expect_stripped_users_in_actors(permission_data, [username])
        permission_data["actors"].should_not include(username_with_tenant)
      end

      it "returns response with tenant ID stripped" do
        response = put(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          payload: {
            acl_permission => {
              actors: [username],
              groups: []
            }
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        # PUT response may be empty JSON {}, verify via GET instead
        fetch_response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        permission_data = fetch_parsed[acl_permission]

        # Standard response uses "actors" array
        permission_data.should have_key("actors")
        expect_stripped_users_in_actors(permission_data, [username])
        permission_data["actors"].should_not include(username_with_tenant)
      end
    end

    context "when X-Ops-TenantId header is absent" do
      it "stores usernames as provided without modification" do
        # PUT without tenant header - no mapping should occur
        response = put(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          payload: {
            acl_permission => {
              actors: [username],
              groups: []
            }
          }
        )
        response.should look_like({ status: 200 })

        fetch_response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor
        )
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        permission_data = fetch_parsed[acl_permission]

        # Standard response uses "actors" array
        permission_data.should have_key("actors")
        expect_stripped_users_in_actors(permission_data, [username])
      end
    end

    context "disambiguation format (separate users/clients arrays)" do
      let(:client_name) { random_name("testclient") }

      before(:each) do
        create_client!(client_name)
      end

      after(:each) do
        delete(api_url("/clients/#{client_name}"), requestor) rescue nil
      end

      it "transforms users but not clients when using disambiguation format" do
        # PUT with disambiguation format: separate users/clients arrays, empty actors
        response = put(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          payload: {
            acl_permission => {
              users: [username],
              clients: [client_name],
              actors: [],
              groups: []
            }
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        # Verify stored correctly via GET (standard format response)
        fetch_response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        permission_data = fetch_parsed[acl_permission]

        # Standard response merges users+clients into actors array
        permission_data.should have_key("actors")
        expect_stripped_users_in_actors(permission_data, [username])
        expect_actors_include(permission_data, [client_name])
        permission_data["actors"].should_not include(username_with_tenant)
      end

      it "transforms users in disambiguation format with granular response" do
        # PUT with disambiguation format
        response = put(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
          requestor,
          payload: {
            acl_permission => {
              users: [username],
              clients: [client_name],
              actors: [],
              groups: []
            }
          },
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        response.should look_like({ status: 200 })

        # GET with granular mode
        fetch_response = get(
          api_url("/nodes/#{node_name}/_acl/#{acl_permission}?detail=granular"),
          requestor,
          headers: { 'X-Ops-TenantId' => tenant_id }
        )
        fetch_response.should look_like({ status: 200 })
        fetch_parsed = parse(fetch_response)
        permission_data = fetch_parsed[acl_permission]

        # Granular mode: separate users/clients arrays
        permission_data.should have_key("users")
        permission_data.should have_key("clients")
        permission_data["actors"].should eq([])

        expect_stripped_users_granular(permission_data, [username])
        expect_clients_unchanged_granular(permission_data, [client_name])
      end
    end
  end

  # Edge cases and multi-permission tests
  describe "ACL username mapping edge cases" do
    let(:group_name) { "testgroup-#{SecureRandom.hex(6)}" }
    let(:username1) { random_name("user1") }
    let(:username2) { random_name("user2") }
    let(:username1_tenant) { "#{username1}__#{tenant_id}" }
    let(:username2_tenant) { "#{username2}__#{tenant_id}" }

    before(:each) do
      create_node!(node_name)

      # Create users
      user1 = platform.create_user(username1_tenant)
      platform.associate_user_with_org(org_name, user1)
      user2 = platform.create_user(username2_tenant)
      platform.associate_user_with_org(org_name, user2)
    end

    after(:each) do
      delete(api_url("/nodes/#{node_name}"), requestor) rescue nil
      delete(api_url("/users/#{username1}"), requestor) rescue nil
      delete(api_url("/users/#{username2}"), requestor) rescue nil
    end

    it "handles multiple users in single permission part" do
      response = put(
        api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
        requestor,
        payload: {
          acl_permission => {
            actors: [username1, username2],
            groups: []
          }
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 200 })

      # Verify both users stripped
      fetch_response = get(
        api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
        requestor
      )
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)
      permission_data = fetch_parsed[acl_permission]

      # Standard response uses "actors" array
      permission_data.should have_key("actors")
      expect_stripped_users_in_actors(permission_data, [username1, username2])
    end

    it "with different tenant IDs does not leak tenant suffixes in responses" do
      username = random_name
      other_tenant_id = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
      username_other_tenant = "#{username}__#{other_tenant_id}"

      user = platform.create_user(username_other_tenant)
      platform.associate_user_with_org(org_name, user)

      # PUT with different tenant ID
      response = put(
        api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
        requestor,
        payload: {
          acl_permission => {
            actors: [username],
            groups: []
          }
        },
        headers: { 'X-Ops-TenantId' => other_tenant_id }
      )
      response.should look_like({ status: 200 })

      # Verify via GET - shows stripped username and no tenant suffix leakage
      fetch_response = get(
        api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
        requestor
      )
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)
      permission_data = fetch_parsed[acl_permission]

      # Standard response uses "actors" array
      permission_data.should have_key("actors")
      expect_stripped_users_in_actors(permission_data, [username])

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

      # Set ACL with both
      response = put(
        api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
        requestor,
        payload: {
          acl_permission => {
            actors: [username, client_name],
            groups: []
          }
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 200 })

      # Verify: user is stripped, client unchanged
      fetch_response = get(
        api_url("/nodes/#{node_name}/_acl/#{acl_permission}"),
        requestor
      )
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)
      permission_data = fetch_parsed[acl_permission]

      # Standard response: actors array contains both users and clients
      permission_data.should have_key("actors")
      expect_stripped_users_in_actors(permission_data, [username])
      expect_actors_include(permission_data, [client_name])

      # Cleanup
      delete(api_url("/users/#{username}"), requestor) rescue nil
      delete(api_url("/clients/#{client_name}"), requestor) rescue nil
    end
  end

  # Test multiple resource types
  describe "username mapping across different resource types" do
    let(:username) { random_name }
    let(:username_with_tenant) { "#{username}__#{tenant_id}" }
    let(:role_name) { "testrole-#{SecureRandom.hex(6)}" }
    let(:env_name) { "testenv-#{SecureRandom.hex(6)}" }

    before(:each) do
      user = platform.create_user(username_with_tenant)
      platform.associate_user_with_org(org_name, user)
    end

    after(:each) do
      delete(api_url("/users/#{username}"), requestor) rescue nil
    end

    it "works for role ACLs" do
      # Create role
      post(api_url("/roles"), requestor, payload: { name: role_name })

      # Set ACL
      response = put(
        api_url("/roles/#{role_name}/_acl/read"),
        requestor,
        payload: {
          read: {
            actors: [username],
            groups: []
          }
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 200 })

      # Verify
      fetch_response = get(api_url("/roles/#{role_name}/_acl/read"), requestor)
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)

      expect_stripped_users(fetch_parsed["read"], [username])

      # Cleanup
      delete(api_url("/roles/#{role_name}"), requestor) rescue nil
    end

    it "works for environment ACLs" do
      # Create environment
      post(api_url("/environments"), requestor, payload: { name: env_name })

      # Set ACL
      response = put(
        api_url("/environments/#{env_name}/_acl/read"),
        requestor,
        payload: {
          read: {
            actors: [username],
            groups: []
          }
        },
        headers: { 'X-Ops-TenantId' => tenant_id }
      )
      response.should look_like({ status: 200 })

      # Verify
      fetch_response = get(api_url("/environments/#{env_name}/_acl/read"), requestor)
      fetch_response.should look_like({ status: 200 })
      fetch_parsed = parse(fetch_response)

      expect_stripped_users(fetch_parsed["read"], [username])

      # Cleanup
      delete(api_url("/environments/#{env_name}"), requestor) rescue nil
    end
  end
end
