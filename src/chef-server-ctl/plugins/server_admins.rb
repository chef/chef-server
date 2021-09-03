#
# Copyright 2015 Chef Software, Inc.
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
#

require "restclient"
require "json"
require "pg"

PLACEHOLDER_GLOBAL_ORG_ID = "00000000000000000000000000000000"

add_command_under_category "grant-server-admin-permissions", "server-admins", "Grant a user the ability to create other users by adding the user to the server-admins group.", 2 do

  cmd_args = ARGV[1..-1]
  if cmd_args.length != 1
    msg = "Username is the only argument to grant-server-admin-permissions.\nPlease pass a single argument."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  username = cmd_args[0]

  db = setup_erchef_db
  server_admins_authz_id = get_server_admins_authz_id(db)
  user_authz_id = get_user_authz_id(db, username)

  # put the user in the server-admins authz group
  req_params = {
    method: :put,
    url: "#{::ChefServerCtl::Config.bifrost_url}/groups/#{server_admins_authz_id}/actors/#{user_authz_id}",
    body: "{}",
    headers: {
      :content_type => :json,
      :accept => :json,
      "X-Ops-Requesting-Actor-Id" => ::ChefServerCtl::Config.bifrost_superuser_id,
    },
  }.merge(::ChefServerCtl::Config.ssl_params)
  RestClient::Request.execute(req_params)

  puts "User #{username} was added to server-admins. This user can now list, read, create, and delete users (even for orgs they are not members of) for this Chef Server."
end

add_command_under_category "remove-server-admin-permissions", "server-admins", "Remove all special permission granted to a user from being a server-admin.", 2 do

  cmd_args = ARGV[1..-1]
  if cmd_args.length != 1
    msg = "Username is the only argument to remove-server-admin-permissions.\nPlease pass a single argument."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  username = cmd_args[0]

  if username.downcase == "pivotal"
    msg = "You cannot remove the base superuser pivotal from server-admins."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  db = setup_erchef_db
  server_admins_authz_id = get_server_admins_authz_id(db)
  user_authz_id = get_user_authz_id(db, username)

  # put the user in the server-admins authz group
  req_params = {
    method: :get,
    url: "#{::ChefServerCtl::Config.bifrost_url}/groups/#{server_admins_authz_id}",
    headers: {
      :content_type => :json,
      :accept => :json,
      "X-Ops-Requesting-Actor-Id" => ::ChefServerCtl::Config.bifrost_superuser_id,
    },
  }.merge(::ChefServerCtl::Config.ssl_params)
  results = JSON.parse(RestClient::Request.execute(req_params))

  users = db.exec_params("SELECT * from USERS WHERE authz_id IN #{create_sql_collection_string(results["actors"])}")
  user_found = false
  users.each do |user|
    if username == user["username"]
      user_found = true
      break
    end
  end

  unless user_found
    msg = "User #{username} is not a member of server-admins so it cannot be removed."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  req_params[:method] = :delete
  req_params[:url] = "#{::ChefServerCtl::Config.bifrost_url}/groups/#{server_admins_authz_id}/actors/#{user_authz_id}"
  RestClient::Request.execute(req_params)

  puts "User #{username} was removed from server-admins. This user can no longer list, read, create, and delete users for this Chef Server except for where they have default permissions (such as within an org)."

end

add_command_under_category "list-server-admins", "server-admins", "List users that have server-admins permissions.", 2 do
  cmd_args = ARGV[1..-1]
  if cmd_args.length != 0
    msg = "This command does not accept arguments."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  db = setup_erchef_db
  server_admins_authz_id = get_server_admins_authz_id(db)

  # get all the user authz_ids for all members of the server-admins authz group
  req_params = {
    method: :get,
    url: "#{::ChefServerCtl::Config.bifrost_url}/groups/#{server_admins_authz_id}",
    headers: {
      :content_type => :json,
      :accept => :json,
      "X-Ops-Requesting-Actor-Id" => ::ChefServerCtl::Config.bifrost_superuser_id,
    },
  }.merge(::ChefServerCtl::Config.ssl_params)
  results = JSON.parse(RestClient::Request.execute(req_params))

  # get the user's authz id
  users = db.exec_params("SELECT * from USERS WHERE authz_id IN #{create_sql_collection_string(results["actors"])}")
  users.each do |user|
    puts user["username"]
  end
end

# returns string in format '('item1','item2',...,'itemN')'
def create_sql_collection_string(arr)
  str = arr.join("','")
  "('#{str}')"
end

def setup_erchef_db
  ::PG::Connection.open(::ChefServerCtl::Config.erchef_sql_connuri)
end

def get_server_admins_authz_id(db)
  server_admins_erchef_group = db.exec_params("SELECT authz_id FROM groups WHERE name='server-admins' AND org_id='#{PLACEHOLDER_GLOBAL_ORG_ID}'")

  if server_admins_erchef_group.ntuples == 0
    msg = "The server-admins global group was not found. Please finish upgrading your Chef Server by following the documentation before using Server Admins related commands."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  if server_admins_erchef_group.ntuples != 1
    msg = "More than one server-admins global group was found. Please contact a sysadmin or support (#{server_admins_erchef_group.ntuples} groups found)."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  server_admins_erchef_group.first["authz_id"]
end

def get_user_authz_id(db, username)
  user = db.exec_params("SELECT authz_id FROM users WHERE username='#{username}'")

  if user.ntuples != 1
    msg = "User #{username} was not found."
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  user.first["authz_id"]
end
