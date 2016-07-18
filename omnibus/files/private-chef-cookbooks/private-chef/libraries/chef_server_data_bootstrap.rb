# Author:: Tyler Cloke <tyler@chef.io>
# Author:: Marc Paradise <marc@chef.io>
# Copyright:: Copyright (c) 2014-5 Chef, Inc.
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

require 'restclient'
require 'json'

class ChefServerDataBootstrap

  GLOBAL_ORG_ID = "00000000000000000000000000000000"
  attr_reader :bifrost, :superuser_authz_id, :bootstrap_time, :node, :server_admins_authz_id

  def initialize(node)
    @node = node
    @bootstrap_time = Time.now.utc.to_s
    @bifrost = node['private_chef']['oc_bifrost']
  end


  def bifrost_superuser_id
      @superuser_id ||= PrivateChef.credentials.get('oc_bifrost', 'superuser_id')
  end

  def bootstrap
    # This is done in two steps - we'll first create the bifrost objects and
    # dependencies.  If this fails, it can be re-run idempotently without
    # risk of causing the run to fail.
    @superuser_authz_id = create_actor_in_authz(bifrost_superuser_id)
    users_authz_id = create_container_in_authz(superuser_authz_id)
    orgs_authz_id = create_container_in_authz(superuser_authz_id)
    create_server_admins_global_group_in_bifrost(users_authz_id, orgs_authz_id)

    # put pivotal in server-admins global group
    insert_authz_actor_into_group(server_admins_authz_id, superuser_authz_id)

    # Now that bifrost operations are complete, create the corresponding
    # objects in erchef.  By separating them, we increase the chance that a
    # bootstrap failed due to an error out of bifrost can be recovered
    # by re-running it.
    username = node['private_chef']['opscode-erchef']['sql_user']
    password = PrivateChef.credentials.get('opscode_erchef', 'sql_password')
    EcPostgres.with_connection(node, 'opscode_chef',
                               'db_superuser' => username,
                               'db_superuser_password' => password) do |conn|
      create_superuser_in_erchef(conn)
      create_server_admins_global_group_in_erchef(conn)
      create_global_container_in_erchef(conn, 'organizations', orgs_authz_id)
      create_global_container_in_erchef(conn, 'users', users_authz_id)
    end
  end

  private

  # Create and set up permissions for the server admins group.
  def create_server_admins_global_group_in_bifrost(users_authz_id, orgs_authz_id)
    @server_admins_authz_id = create_group_in_authz(bifrost['superuser_id'])
    %w{create read update delete}.each do |permission|
      # grant server admins group permission on the users and orgs container,
      # as the erchef superuser.
      grant_authz_object_permission(permission, "groups", "containers", users_authz_id,
                                    server_admins_authz_id, superuser_authz_id)
      # NOTE: org creator ignores the org container perms anyway, so this is really only used
      # for list and create.
      grant_authz_object_permission(permission, "groups", "containers", orgs_authz_id,
                                    server_admins_authz_id, superuser_authz_id)

      # grant superuser actor permissions on the server admin group,
      # as the bifrost superuser
      grant_authz_object_permission(permission, "actors", "groups", server_admins_authz_id,
                                    superuser_authz_id, bifrost_superuser_id)
    end

    # Grant server-admins read permissions on itself as the bifrost superuser.
    grant_authz_object_permission("read", "groups", "groups", server_admins_authz_id,
                                  server_admins_authz_id, bifrost_superuser_id)
  end

  # Insert the server admins global group into the erchef groups table.
  def create_server_admins_global_group_in_erchef(conn)
    simple_insert(conn, 'groups',
                  id: SecureRandom.uuid.gsub("-", ""),
                  org_id: GLOBAL_ORG_ID,
                  authz_id: server_admins_authz_id,
                  name: 'server-admins',
                  last_updated_by: superuser_authz_id,
                  created_at: bootstrap_time,
                  updated_at: bootstrap_time)
  end

  # insert the erchef superuser's key into the erchef keys table,
  # and the user record into the users table.
  def create_superuser_in_erchef(conn)
    require 'openssl'

    raw_key = PrivateChef.credentials.get('chef-server', 'superuser_key')
    public_key = OpenSSL::PKey::RSA.new(raw_key).public_key.to_s

    user_id = SecureRandom.uuid.gsub("-", "")
    simple_insert(conn, 'keys',
                    id: user_id,
                    key_name: 'default',
                    public_key: public_key,
                    key_version: 0,
                    created_at: bootstrap_time,
                    expires_at: "infinity")

    simple_insert(conn, 'users',
                    id: user_id,
                    username: 'pivotal',
                    email: 'root@localhost.localdomain',
                    authz_id: @superuser_authz_id,
                    created_at: bootstrap_time,
                    updated_at: bootstrap_time,
                    last_updated_by: bifrost_superuser_id,
                    pubkey_version: 0, # Old constrant requires it to be not-null
                    serialized_object: JSON.generate(
                      first_name: "Chef",
                      last_name: "Server",
                      display_name: "Chef Server Superuser"))
  end

  def create_global_container_in_erchef(conn, name, authz_id)
    simple_insert(conn, 'containers',
                    id: authz_id, # TODO is this right?
                    name: name,
                    authz_id: authz_id,
                    org_id: GLOBAL_ORG_ID,
                    last_updated_by: superuser_authz_id,
                    created_at: bootstrap_time,
                    updated_at: bootstrap_time)
  end

  # db helper to construct and execute a simple insert statement
  def simple_insert(conn, table, fields)
    placeholders = []
    1.upto(fields.length) { |x| placeholders << "$#{x}" }
    placeholders.join(", ")
    conn.exec_params("INSERT INTO #{table} (#{fields.keys.join(", ")}) VALUES (#{placeholders.join(", ")})",
                     fields.values) # confirm ordering


  end

  ## Bifrost access helpers.

  def create_group_in_authz(requestor_id)
    create_object_in_authz("groups", requestor_id)
  end

  def create_actor_in_authz(requestor_id)
    create_object_in_authz("actors", requestor_id)
  end

  def create_container_in_authz(requestor_id)
    create_object_in_authz("containers", requestor_id)
  end

  def create_object_in_authz(object_name, requestor_id)
    result = bifrost_request(:post, "#{object_name}", "{}", requestor_id)
    JSON.parse(result)["id"]
  end

  # Tells bifrost that an actor is a member of a group.
  # Group membership is managed through bifrost, and not via erchef.
  def insert_authz_actor_into_group(group_id, actor_id)
    bifrost_request(:put, "/groups/#{group_id}/actors/#{actor_id}", "{}", superuser_authz_id)
  end

  def grant_authz_object_permission(permission_type, granted_to_object_type, granted_on_object_type, granted_on_id, granted_to_id, requestor_id)
    url = "#{granted_on_object_type}/#{granted_on_id}/acl/#{permission_type}"
    body = JSON.parse(bifrost_request(:get, url, nil, requestor_id))
    body[granted_to_object_type] << granted_to_id
    bifrost_request(:put, url, body.to_json, requestor_id)
  end

  # Assemble the appropriate header per bifrost's expectations
  # This automatically retries any failed request.  It is not uncommon
  # for bifrost to be unavailable when we're ready to start, as
  # it's still spinning up.
  #
  def bifrost_request(method, rel_path, body, requestor_id)
    headers = {
      :content_type => :json,
      :accept => :json,
      'X-Ops-Requesting-Actor-Id' => requestor_id
    }
    retries = 5
    begin
      if method == :get
        RestClient.get("http://#{bifrost['vip']}:#{bifrost['port']}/#{rel_path}", headers)
      else
        RestClient.send(method, "http://#{bifrost['vip']}:#{bifrost['port']}/#{rel_path}",  body, headers)
      end
    rescue RestClient::Exception, Errno::ECONNREFUSED => e
      error = e.respond_to?(:response) ? e.response.chomp : e.message
      if retries > 0
        sleep_time = 2**((5 - retries))
        retries -= 1
        Chef::Log.warn "Error from bifrost: #{error}, retrying after #{sleep_time}s. Retries remaining: #{retries}"
        sleep sleep_time
        retry
      else
        Chef::Log.error "Error from bifrost: #{error}, retries have been exhausted"
        raise
      end
    end
  end
end

  
