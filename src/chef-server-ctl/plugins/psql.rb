#
# Copyright:: Copyright (c) 2015 Chef Software, Inc.
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

require "json"

known_dbs = {
  "opscode_chef" => { "dbname" => "opscode_chef", "config_key" => "opscode-erchef", "hashseed" => "private_chef" },
  "bifrost" => { "dbname" => "bifrost", "config_key" => "oc_bifrost", "hashseed" => "private_chef" },
  "oc_id" => { "dbname" => "oc_id", "config_key" => "oc_id", "hashseed" => "private_chef" },
  "bookshelf" => { "dbname" => "bookshelf", "config_key" => "bookshelf", "hashseed" => "private_chef" },
  "push-jobs" => { "dbname" => "opscode_pushy", "config_key" => "postgresql", "hashseed" => "pushy" },
  "reporting" => { "dbname" => "opscode_reporting", "config_key" => "postgresql", "hashseed" => "reporting" },
}

# Alias better-known service names for ease of use:
known_dbs["oc_erchef"] = known_dbs["opscode_chef"]
known_dbs["oc-id"] = known_dbs["oc_id"]
known_dbs["opscode-erchef"] = known_dbs["opscode_chef"]

add_command_under_category "psql", "Database", "Launches an interactive PostgreSQL psql session with the service database you name. Add '--write' for write access and '--options <OPTIONS>' to specify psql options.", 2 do

  service_name = ARGV[1]
  write_arg = "--write"
  options_arg = "--options"
  if ARGV.include?(options_arg)
    psql_options = " #{ARGV[ARGV.index(options_arg) + 1]}"
  end

  known_db_names = known_dbs.keys.sort.join(", ")

  if service_name.nil?
    STDERR.puts "[ERROR] You must supply a service name. Valid names include: #{known_db_names}"
    exit 1
  elsif !known_dbs[service_name]
    STDERR.puts "[ERROR] #{service_name} does not appear to be a valid service name. Valid names include: #{known_db_names}."
    exit 1
  elsif running_config.nil?
    STDERR.puts "[ERROR] Cannot connect to database if you haven't completed a reconfigure"
    exit 1
  end

  db_config = running_config

  if File.exist?("/etc/opscode-push-jobs-server/opscode-push-jobs-server-running.json")
    db_config.merge!(JSON.parse(File.read("/etc/opscode-push-jobs-server/opscode-push-jobs-server-running.json")))
  end
  if File.exist?("/etc/opscode-reporting/opscode-reporting-running.json")
    db_config.merge!(JSON.parse(File.read("/etc/opscode-reporting/opscode-reporting-running.json")))
  end

  seed =        known_dbs[service_name]["hashseed"]
  db_hash_key = known_dbs[service_name]["config_key"]
  db_name =     known_dbs[service_name]["dbname"]
  db_host =     db_config[seed]["postgresql"]["vip"]
  db_port =     db_config[seed]["postgresql"]["port"]

  # undocumented, but available. Intended for use primarily for
  # gather-logs to be able to do its thing correctly.
  if ARGV.include? "--as-admin"
    cfg = running_service_config("postgresql")
    db_username = cfg["db_connection_superuser"] || cfg["db_superuser"]
    db_password = credentials.get("postgresql", "db_superuser_password")
  else
    if ARGV.include? write_arg
      db_username = db_config[seed][db_hash_key]["sql_connection_user"] || db_config[seed][db_hash_key]["sql_user"]
      db_password_string = "sql_password"
    else
      db_username = db_config[seed][db_hash_key]["sql_ro_user"] + (db_config[seed][db_hash_key]["sql_connection_user"] ? "@" + db_host : "")
      db_password_string = "sql_ro_password"
    end
    # Sorry.
    db_hash_key = "opscode_erchef" if db_hash_key == "opscode-erchef"
    db_password = credentials.get(db_hash_key, db_password_string)
  end

  if ARGV.include?("--debug") || ARGV.include?("-vv")
    STDOUT.puts "Host: #{db_host}"
    STDOUT.puts "Port: #{db_port}"
    STDOUT.puts "Username: #{db_username}"
    STDOUT.puts "Password: #{db_password}"
    STDOUT.puts "DBName: #{db_name}"
  end

  ENV["PGPASSWORD"] = db_password
  ENV["PAGER"] = "less"
  ENV["LESS"] = "-iMSx4 -FX"
  cmd = "/opt/opscode/embedded/bin/psql --host #{db_host} --username #{db_username} --port #{db_port} --dbname #{db_name}#{psql_options}"
  exec cmd
end
