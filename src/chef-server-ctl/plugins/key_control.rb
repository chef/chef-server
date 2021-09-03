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

require "openssl"
require "optparse"
require "ostruct"
require "chef/key"

# Due to how things are being exec'ed, the CWD will be all wrong,
# so we want to use the full path when loaded from omnibus-ctl,
# but we need the local relative path for it to work with rspec
# TODO Check if this is still true in gem-world
require "chef_server_ctl/helpers/key_ctl_helper"

add_command_under_category "add-client-key", "key-rotation", "Create a new client key", 2 do
  cmd_args = ARGV[1..-1]
  @helper = ::ChefServerCtl::Helpers::KeyCtlHelper.new
  @options = OpenStruct.new
  @options.expiration_date = "infinity"
  @key = nil
  @usage = "Usage: chef-server-ctl add-client-key ORGNAME CLIENTNAME [-p, --public-key-path, -e, --expiration-date DATE, -k, --key-name NAME]."
  @usage = @usage << @helper.add_key_usage
  @arg_list = ["-e", "--expiration-date", "-k", "--key-name", "-p", "--public-key-path"]

  opt_parser = OptionParser.new do |opts|
    opts.banner = @usage

    opts.on("-e", "--expiration-date DATE", "Expiration date for your key (if desired).") do |expiration_date|
      @helper.catch_argument_passed_as_input(@arg_list, "--expiration-date", expiration_date)
      @helper.check_valid_iso_date(expiration_date)
      @options.expiration_date = expiration_date
    end

    opts.on("-k", "--key-name NAME", "Name for your key (defaults to fingerprint of your key).") do |key_name|
      @helper.catch_argument_passed_as_input(@arg_list, "--key-name", key_name)
      @options.key_name = key_name
    end

    opts.on("-p", "--public-key-path PATH", "Path to a valid public key, if not passed, the server will generate a public key for you.") do |public_key_path|
      @helper.catch_argument_passed_as_input(@arg_list, "--public-key-path", public_key_path)
      @options.public_key_path = public_key_path
    end
  end

  begin
    opt_parser.parse!(ARGV[1..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :orgname, "ORGNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :clientname, "CLIENTNAME", 2)

  if @options.public_key_path
    @key = @helper.read_and_check_key(@options.public_key_path)
  else
    if @options.key_name
      @key = nil
    else
      @helper.exit_failure(@helper.pass_key_name_if_public_key_missing)
    end
  end

  Chef::Config[:chef_server_url] = "#{Chef::Config[:chef_server_root]}/organizations/#{@options.orgname}"
  chef_key = @helper.populate_client_key(@options.clientname, @options.key_name, @key, @options.expiration_date)

  begin
    new_key = chef_key.create
    if new_key.private_key
      @helper.print_private_key(@options.key_name, new_key.private_key)
    end
  rescue Net::HTTPServerException => e
    if e.response.code == "409"
      @helper.exit_failure("Error: A key named #{@options.key_name} already exists for #{@options.clientname} in org #{@options.orgname}.")
    elsif e.response.code == "404"
      @helper.exit_failure("Error: Could not find client #{@options.clientname} in org #{@options.orgname}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "add-user-key", "key-rotation", "Create a new user key", 2 do
  cmd_args = ARGV[1..-1]
  @helper = ::ChefServerCtl::Helpers::KeyCtlHelper.new
  @options = OpenStruct.new
  @options.expiration_date = "infinity"
  @key = nil
  @usage = "Usage: chef-server-ctl add-user-key USERNAME [-p, --public-key-path, -e, --expiration-date DATE, -k, --key-name NAME]"
  @usage = @usage << @helper.add_key_usage
  @arg_list = ["-e", "--expiration-date", "-k", "--key-name", "-p", "--public-key-path"]
  opt_parser = OptionParser.new do |opts|
    opts.banner = @usage

    opts.on("-e", "--expiration-date DATE", "Expiration date for your key (if desired).") do |expiration_date|
      @helper.catch_argument_passed_as_input(@arg_list, "--expiration-date", expiration_date)
      @helper.check_valid_iso_date(expiration_date)
      @options.expiration_date = expiration_date
    end

    opts.on("-k", "--key-name NAME", "Name for your key (defaults to fingerprint of your key).") do |key_name|
      @helper.catch_argument_passed_as_input(@arg_list, "--key-name", key_name)
      @options.key_name = key_name
    end

    opts.on("-p", "--public-key-path PATH", "Path to a valid public key, if not passed, the server will generate a public key for you.") do |public_key_path|
      @helper.catch_argument_passed_as_input(@arg_list, "--public-key-path", public_key_path)
      @options.public_key_path = public_key_path
    end
  end

  begin
    opt_parser.parse!(ARGV[1..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :username, "USERNAME", 1)

  if @options.public_key_path
    @key = @helper.read_and_check_key(@options.public_key_path)
  else
    if @options.key_name
      @key = nil
    else
      @helper.exit_failure(@helper.pass_key_name_if_public_key_missing)
    end
  end

  chef_key = @helper.populate_user_key(@options.username, @options.key_name, @key, @options.expiration_date)
  begin
    new_key = chef_key.create
    if new_key.private_key
      @helper.print_private_key(@options.key_name, new_key.private_key)
    end
  rescue Net::HTTPServerException => e
    if e.response.code == "409"
      @helper.exit_failure("Error: A key named #{@options.key_name} already exists for user #{@options.username}.")
    elsif e.response.code == "404"
      @helper.exit_failure("Error: Could not find user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "list-client-keys", "key-rotation", "List keys for a client", 2 do
  cmd_args = ARGV[1..-1]
  @helper = ::ChefServerCtl::Helpers::KeyCtlHelper.new
  @options = OpenStruct.new
  @options.show_public_keys = false
  @usage = "Usage: chef-server-ctl list-client-keys ORGNAME CLIENTNAME [-v, --verbose]"
  @arg_list = ["-v", "--verbose"]

  opt_parser = OptionParser.new do |opts|
    opts.on("-v", "--verbose", "Whether or not to output the full public key.") do
      @options.verbose = true
    end
  end

  begin
    opt_parser.parse!(ARGV[1..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :orgname, "ORGNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :clientname, "CLIENTNAME", 2)

  Chef::Config[:chef_server_url] = "#{Chef::Config[:chef_server_root]}/organizations/#{@options.orgname}"
  begin
    if @options.verbose
      @helper.output_full_key_results(Chef::Key.list_by_client(@options.clientname, inflate = true))
    else
      @helper.output_simple_key_results(Chef::Key.list_by_client(@options.clientname))
    end
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find client #{@options.clientname} in org #{@options.orgname}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "list-user-keys", "key-rotation", "List keys for a user", 2 do
  cmd_args = ARGV[1..-1]
  @helper = ::ChefServerCtl::Helpers::KeyCtlHelper.new

  @options = OpenStruct.new
  @options.show_public_keys = false
  @usage = "Usage: chef-server-ctl list-user-keys USERNAME [-v, --verbose]"
  @arg_list = ["-v", "--verbose"]

  opt_parser = OptionParser.new do |opts|
    opts.on("-v", "--verbose", "Outputs more verbose key info.") do
      @options.verbose = true
    end
  end

  begin
    opt_parser.parse!(ARGV[1..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :username, "USERNAME", 1)

  begin
    if @options.verbose
      @helper.output_full_key_results(Chef::Key.list_by_user(@options.username, inflate = true))
    else
      @helper.output_simple_key_results(Chef::Key.list_by_user(@options.username))
    end
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "delete-user-key", "key-rotation", "Delete a key", 2 do
  cmd_args = ARGV[1..-1]
  @helper = ::ChefServerCtl::Helpers::KeyCtlHelper.new
  @options = OpenStruct.new
  @usage = "Usage: chef-server-ctl delete-user-key USERNAME KEYNAME"

  @helper.get_required_arg!(@options, cmd_args, @usage, :username, "USERNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :key_name, "KEYNAME", 2)

  key = Chef::Key.new(@options.username, "user")
  key.name @options.key_name
  begin
    key.destroy
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find key #{@options.key_name} for user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "delete-client-key", "key-rotation", "Delete a key", 2 do
  cmd_args = ARGV[1..-1]
  @helper = ::ChefServerCtl::Helpers::KeyCtlHelper.new
  @options = OpenStruct.new
  @usage = "Usage: chef-server-ctl delete-client-key ORGNAME CLIENTNAME KEYNAME"

  @helper.get_required_arg!(@options, cmd_args, @usage, :orgname, "ORGNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :clientname, "CLIENTNAME", 2)
  @helper.get_required_arg!(@options, cmd_args, @usage, :key_name, "KEYNAME", 3)

  Chef::Config[:chef_server_url] = "#{Chef::Config[:chef_server_root]}/organizations/#{@options.orgname}"
  key = Chef::Key.new(@options.clientname, "client")
  key.name @options.key_name
  begin
    key.destroy
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find key #{@options.key_name} for user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end
