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

require "sequel"
require "openssl"
require "time"

cmd_args = ARGV[3..-1]

##################
# Helper Classes #
##################
class Database
  attr_reader :db
  def initialize(running_config)
    if running_config.nil?
      STDERR.puts "Error: you must run chef-server-ctl reconfigure before using key commands."
      exit 1
    end

    # Pull all the DB params from config; the database name seems to be hardcoded,
    # however -- I found it in the postgresql.rb recipe in the private-chef cookbooks;
    # I'm not sure why we hardcode it instead of making it configurable, but I assume
    # there are reasons?
    db_user = running_config['private_chef']['postgresql']['sql_user']
    db_password = running_config['private_chef']['postgresql']['sql_password']
    db_host = running_config['private_chef']['postgresql']['listen_address']

    # Set convert_infinite_timestamps to a float so that Sequel
    # doesn't crash out on querying an infinity timestamp.
    # See this commit for more detail:
    # https://github.com/jeremyevans/sequel/commit/2440e98b4a7063a90d8337e57b1c3e3d3a1cbd0b
    @db = Sequel.connect(:adapter => 'postgres', :host => db_host,
                         :database => 'opscode_chef', :user => db_user,
                         :password => db_password, :convert_infinite_timestamps => :float)

  end
end

class PublicKeyOutputter
  def output_key_results(results, hide_public_keys, database)
    results.each do |row|
      # get expiration date from keys table as
      # results variable is result of keys_by_name view
      expires_at = database.db[:keys].where(:id => row[:id]).where(:key_name => row[:key_name]).first[:expires_at]
      puts "\nkey_name: #{row[:key_name]}"
      puts "expires_at: #{expires_at}"
      unless hide_public_keys
        puts "public_key:"
        puts "#{row[:public_key]}"
      end
    end
  end
end

class ArgParser
  def find_arg_after_identifier(args, identifier)
    result = nil
    args.each_with_index do |arg, i|
      if arg == identifier
        result = args[i+1]
        break
      end
    end
    result
  end
end

class AddKey
  def check_org_args(cmd_args, usage_msg)
    unless cmd_args.length == 2 || \
      (cmd_args.length == 4 && (cmd_args[2] == "--expiration-date" || cmd_args[2] == "--key-name")) || \
      (cmd_args.length == 6 && ((cmd_args[2] == "--expiration-date" && cmd_args[4] == "--key-name") || (cmd_args[4] == "--expiration-date" && cmd_args[2] == "--key-name")))
      STDERR.puts usage_msg
      STDERR.puts "Usage: Expiration date defaults to infinite. Pass an ISO 8601 fomatted string: YYYY-MM-DDTHH:MM:SS e.g. 2013-12-24T21:00:00"
      STDERR.puts "Usage: Default name used is the fingerprint of the key passed"
      exit 1
    end
  end

  def check_client_args(cmd_args, usage_msg)
    unless cmd_args.length == 3 || \
      (cmd_args.length == 5 && (cmd_args[3] == "--expiration-date" || cmd_args[3] == "--key-name")) || \
      (cmd_args.length == 7 && ((cmd_args[3] == "--expiration-date" && cmd_args[5] == "--key-name") || (cmd_args[5] == "--expiration-date" && cmd_args[3] == "--key-name")))
      STDERR.puts usage_msg
      STDERR.puts "Usage: Expiration date defaults to infinite. Pass an ISO 8601 fomatted string: YYYY-MM-DDTHH:MM:SS e.g. 2013-12-24T21:00:00 in UTC timezone"
      STDERR.puts "Usage: Default name used is the fingerprint of the key passed"
      exit 1
    end
  end

  def add_key(cmd_args, id, database)
    # this is guaranteed to work because it would have crashed above if not
    # expiration_date and key_name will be nil if they weren't passed
    arg_parser = ArgParser.new
    expiration_date = arg_parser.find_arg_after_identifier(cmd_args, "--expiration-date")
    key_name        = arg_parser.find_arg_after_identifier(cmd_args, "--key-name")
    objectname = cmd_args[0]
    keypath = cmd_args[1]
    key = File.read(keypath)

    unless /-----BEGIN PUBLIC KEY-----/.match(key)
      STDERR.puts "Error: Invalid public key passed. Key must begin with:"
      STDERR.puts "Error: -----BEGIN PUBLIC KEY-----"
      exit 1
    end

    # if a key_name was not specified, use the fingerprint of the key
    unless key_name
      begin
        openssl_key_object = OpenSSL::PKey::RSA.new(key)
        data_string = OpenSSL::ASN1::Sequence([
                                               OpenSSL::ASN1::Integer.new(openssl_key_object.public_key.n),
                                               OpenSSL::ASN1::Integer.new(openssl_key_object.public_key.e)
                                              ])
        key_name = OpenSSL::Digest::SHA1.hexdigest(data_string.to_der).scan(/../).join(':')
      rescue
        STDERR.puts "Error: Could not parse fingerprint for public key #{keypath}"
        STDERR.puts "Error: If optional --key-name arg is not specified, then fingerprint of key is used by default as the key_name"
        exit 1
      end
    end


    if expiration_date
      begin
        Time.parse(expiration_date)
      rescue
        STDERR.puts "Error: Invalid date format inserted"
        STDERR.puts "Error: You inserted #{expiration_date}"
        STDERR.puts "Error: Please input a date in format YYYY-MM-DDTHH:MM:SS e.g. 2013-12-24T21:00:00"
        STDERR.puts "Error: Alternatively, simply don't pass --expiration-date if you don't want your key to expire"
        exit 1
      end
    else
      expiration_date = "infinity"
    end

    object = {
      'id'          => id,
      'key_name'    => key_name,
      'public_key'  => key,
      'key_version' => 0,
      'created_at'  => Time.now.utc,
      'expires_at'  => expiration_date
    }

    begin
      database.db[:keys].insert(object)
    rescue Sequel::UniqueConstraintViolation
      STDERR.puts "Error: The key_name you passed (#{key_name}) was not unique for your object (#{objectname})"
      exit 1
    end
  end
end

add_command_under_category "add-client-key", "key-rotation", "Create a new client key", 2 do
  database = Database.new(running_config)

  addkey = AddKey.new
  addkey.check_client_args(cmd_args, "Usage: chef-server-ctl add-client-key <orgname> <client> <path_to_public_key> [--expiration-date <date>] [--key-name <string>]")

  organization = cmd_args[0]
  client       = cmd_args[1]

  org_id = nil
  database.db[:orgs].where(:name => organization).each do |row|
    org_id = row[:id]
  end
  unless org_id
    STDERR.puts "Error: organization could not be found for orgname #{organization}"
    exit 1
  end

  id = nil
  database.db[:clients].where(:name => client).where(:org_id => org_id).each do |row|
    id = row[:id]
  end
  unless id
    STDERR.puts "Error: client could not be found for client #{client}"
    exit 1
  end

  # delete orgname from the cmd_args so that it has the same interface as add-user-key
  cmd_args.delete_at(0)

  addkey.add_key(cmd_args, id, database)
end

add_command_under_category "add-user-key", "key-rotation", "Create a new user key", 2 do
  database = Database.new(running_config)

  addkey = AddKey.new
  addkey.check_org_args(cmd_args, "Usage: chef-server-ctl add-user-key <username> <path_to_public_key> [--expiration-date <date>] [--key-name <string>]")

  username = cmd_args[0]

  id = nil
  database.db[:users].where(:username => username).each do |row|
    id = row[:id]
  end
  unless id
    STDERR.puts "Error: user could not be found for username #{username}"
    exit 1
  end
  AddKey.new.add_key(cmd_args, id, database)
end

add_command_under_category "delete-user-key", "key-rotation", "Delete a key", 2 do
  database = Database.new(running_config)

  unless cmd_args.length == 2
    STDERR.puts "Usage: chef-server-ctl delete-user-key <username> <key_name>"
    exit 1
  end

  username = cmd_args[0]
  key_name = cmd_args[1]

  user_rows = database.db[:users].where(:username => username)
  if user_rows.count == 0
    STDERR.puts "Error: Could not find user #{username}"
  end

  user_id = nil
  user_rows.each do |row|
    user_id = row[:id]
  end

  results = database.db[:keys].where(:id => user_id).where(:key_name => key_name)
  if results.count == 0
    STDERR.puts "Error: Could not find key where id #{username} and key_name #{key_name}"
    exit 1
  end

  # this is just a safety check so that we don't accidentally delete multiple rows, should never happen
  if results.count > 1
    STDERR.puts "Error: Multiple keys found for user #{username} and key_name #{key_name}. Please contact support@chef.io with this error message"
    exit 1
  end

  results.delete
end

add_command_under_category "delete-client-key", "key-rotation", "Delete a key", 2 do
  database = Database.new(running_config)

  unless cmd_args.length == 3
    STDERR.puts "Usage: chef-server-ctl delete-client-key <organization> <client> <key_name>"
    exit 1
  end

  orgname    = cmd_args[0]
  clientname = cmd_args[1]
  key_name   = cmd_args[2]

  org_rows = database.db[:orgs].where(:name => orgname)
  if org_rows.count == 0
    STDERR.puts "Error: Could not find organization #{orgname}"
  end
  org_id = nil
  org_rows.each do |row|
    org_id = row[:id]
  end

  client_rows = database.db[:clients].where(:name => clientname).where(:org_id => org_id)
  if client_rows.count == 0
    STDERR.puts "Error: Could not find client #{clientname}"
  end
  client_id = nil
  client_rows.each do |row|
    client_id = row[:id]
  end

  results = database.db[:keys].where(:id => client_id).where(:key_name => key_name)
  if results.count == 0
    STDERR.puts "Error: Could not find key for client #{clientname} in organization #{orgname} and key_name #{key_name}"
    exit 1
  end

  # this is just a safety check so that we don't accidentally delete multiple rows, should never happen
  if results.count > 1
    STDERR.puts "Error: Multiple keys found for client #{clientname} in organization #{orgname} and key_name #{key_name}"
    STDERR.puts "Error: Please contact support@chef.io with this error message"
    exit 1
  end

  results.delete
end

add_command_under_category "list-client-keys", "key-rotation", "List keys for a client", 2 do
  database = Database.new(running_config)

  # allow users to hide public key from output, this will allow for much easier
  # viewing of keys if a user has lots of keys and they just want to see the key_names
  hide_public_keys = (cmd_args[2] == "--hide-public-keys")

  unless cmd_args.length == 2 || (cmd_args.length == 3 && cmd_args[2] == "--hide-public-keys")
    STDERR.puts "Usage: chef-server-ctl list-client-keys <organization> <client> [--hide-public-keys]"
    exit 1
  end

  orgname    = cmd_args[0]
  clientname = cmd_args[1]

  org_id = nil
  database.db[:orgs].where(:name => orgname).each do |row|
    org_id = row[:id]
  end
  unless org_id
    STDERR.puts "Organization not found for organization name #{orgname}"
    exit 1
  end
  results = database.db[:keys_by_name].where(:type => 'client').where(:org_id => org_id).where(:name => clientname)
  if results.count == 0
    puts "Could not find any keys for client #{clientname} in organization #{orgname}"
  else
    puts "#{results.count} total key(s) found for client #{clientname} in organization #{orgname}"
    PublicKeyOutputter.new.output_key_results(results, hide_public_keys, database)
  end
end

add_command_under_category "list-user-keys", "key-rotation", "List keys for a user", 2 do
  database = Database.new(running_config)

  # allow users to hide public key from output, this will allow for much easier
  # viewing of keys if a user has lots of keys and they just want to see the key_names
  hide_public_keys = cmd_args[1] == "--hide-public-keys"

  unless cmd_args.length == 1 || (cmd_args.length == 2 && cmd_args[1] == "--hide-public-keys")
    STDERR.puts "Usage: chef-server-ctl list-user-keys <username> [--hide-public-keys]"
    exit 1
  end

  username = cmd_args[0]

  results = database.db[:keys_by_name].where(:type => 'user').where(:name => username)
  if results.count == 0
    STDERR.puts "Error: Could not find any keys for user #{username}"
    exit 1
  else
    puts "#{results.count} total key(s) found for user #{username}"
    PublicKeyOutputter.new.output_key_results(results, hide_public_keys, database)
  end
end
