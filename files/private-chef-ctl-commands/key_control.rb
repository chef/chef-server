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

# due to how things are being exec'ed, the CWD will be all wrong,
# so we want to use the full path when loaded from omnibus-ctl,
# but we need the local relative path for it to work with rspec
begin
  require "helpers/key_ctl_helper"
rescue LoadError
  require '/opt/opscode/embedded/service/omnibus-ctl/helpers/key_ctl_helper'
end

add_command_under_category "add-client-key", "key-rotation", "Create a new client key", 2 do
  cmd_args = ARGV[3..-1]
  @helper = KeyCtlHelper.new
  @options = OpenStruct.new
  @options.expiration_date = "infinity"
  @key = nil
  @usage = "Usage: chef-server-ctl add-client-key ORGNAME CLIENTNAME PUBLIC_KEY_PATH [-e, --expiration-date DATE] [-k, --key-name NAME]"
  @usage = @usage << @helper.add_key_usage
  @arg_list = ["-e", "--expiration-date", "-k", "--key-name"]

  opt_parser = OptionParser.new do |opts|
    opts.banner = @usage

    opts.on("-e", "--expiration-date DATE", "Expiration date for your key (if desired).") do |expiration_date|
      @helper.catch_argument_passed_as_input(@arg_list, "--expiration-date", expiration_date)
      @helper.check_valid_iso_date(expiration_date)
    end

    opts.on("-k", "--key-name NAME", "Name for your key (defaults to fingerprint of your key).") do |key_name|
      @helper.catch_argument_passed_as_input(@arg_list, "--key-name", key_name)
      @options.key_name = key_name
    end
  end

  begin
    opt_parser.parse!(ARGV[3..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :orgname, "ORGNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :clientname, "CLIENTNAME", 2)
  @helper.get_required_arg!(@options, cmd_args, @usage, :public_key_path, "PUBLIC_KEY_PATH", 3)

  @key = @helper.read_and_check_key(@options.public_key_path)

  # if --key-name was not passed, default to the key's fingerprint
  if @options.key_name.nil?
    @options.key_name = @helper.generate_fingerprint(@key)
  end

  begin
    @helper.post_rest("/organizations/#{@options.orgname}/clients/#{@options.clientname}/keys", @helper.build_key_object(@options.key_name, @key, @options.expiration_date))
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
  cmd_args = ARGV[3..-1]
  @helper = KeyCtlHelper.new
  @options = OpenStruct.new
  @options.expiration_date = "infinity"
  @key = nil
  @usage = "Usage: chef-server-ctl add-user-key USERNAME PUBLIC_KEY_PATH [-e, --expiration-date DATE] [-k, --key-name NAME]"
  @usage = @usage << @helper.add_key_usage
  @arg_list = ["-e", "--expiration-date", "-k", "--key-name"]
  opt_parser = OptionParser.new do |opts|
    opts.banner = @usage

    opts.on("-e", "--expiration-date DATE", "Expiration date for your key (if desired).") do |expiration_date|
      @helper.catch_argument_passed_as_input(@arg_list, "--expiration-date", expiration_date)
      @helper.check_valid_iso_date(expiration_date)
    end

    opts.on("-k", "--key-name NAME", "Name for your key (defaults to fingerprint of your key).") do |key_name|
      @helper.catch_argument_passed_as_input(@arg_list, "--key-name", key_name)
      @options.key_name = key_name
    end
  end

  begin
    opt_parser.parse!(ARGV[3..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :username, "USERNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :public_key_path, "PUBLIC_KEY_PATH", 2)

  @key = @helper.read_and_check_key(@options.public_key_path)

  # if --key-name was not passed, default to the key's fingerprint
  if @options.key_name.nil?
    @options.key_name = @helper.generate_fingerprint(@key)
  end

  begin
    @helper.post_rest("/users/#{@options.username}/keys", @helper.build_key_object(@options.key_name, @key, @options.expiration_date))
  rescue Net::HTTPServerException => e
    if e.response.code == "409"
      @helper.exit_failure("Error: A key named #{@options.key_name} already exists for user #{@options.username}}.")
    elsif e.response.code == "404"
      @helper.exit_failure("Error: Could not find user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "list-client-keys", "key-rotation", "List keys for a client", 2 do
  cmd_args = ARGV[3..-1]
  @helper = KeyCtlHelper.new
  @options = OpenStruct.new
  @options.show_public_keys = false
  @usage = "Usage: chef-server-ctl list-client-keys ORGNAME CLIENTNAME [-s, --show-public-keys]"
  @arg_list = ["-s", "--show-public-keys"]

  opt_parser = OptionParser.new do |opts|
    opts.on("-s", "--show-public-keys", "Whether or not to output the full public key.") do
      @options.show_public_keys = true
    end
  end

  begin
    opt_parser.parse!(ARGV[3..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :orgname, "ORGNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :clientname, "CLIENTNAME", 2)

  begin
    results = @helper.get_rest("/organizations/#{@options.orgname}/clients/#{@options.clientname}/keys")
    @helper.output_key_results(results, @options.show_public_keys)
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find client #{@options.clientname} in org #{@options.orgname}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "list-user-keys", "key-rotation", "List keys for a user", 2 do
  cmd_args = ARGV[3..-1]
  @helper = KeyCtlHelper.new

  @options = OpenStruct.new
  @options.show_public_keys = false
  @usage = "Usage: chef-server-ctl list-user-keys USERNAME [-s, --show-public-keys]"
  @arg_list = ["-s", "--show-public-keys"]

  opt_parser = OptionParser.new do |opts|
    opts.on("-s", "--show-public-keys", "Whether or not to output the full public key.") do
      @options.show_public_keys = true
    end
  end

  begin
    opt_parser.parse!(ARGV[3..-1])
  rescue OptionParser::MissingArgument => e
    @helper.parse_missing_arg_error(e)
  rescue OptionParser::InvalidOption => e
    @helper.parse_invalid_arg_error(e)
  end

  @helper.get_required_arg!(@options, cmd_args, @usage, :username, "USERNAME", 1)

  begin
    results = @helper.get_rest("/users/#{@options.username}/keys")
    @helper.output_key_results(results, @options.show_public_keys)
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "delete-user-key", "key-rotation", "Delete a key", 2 do
  cmd_args = ARGV[3..-1]
  @helper = KeyCtlHelper.new
  @options = OpenStruct.new
  @usage = "Usage: chef-server-ctl delete-user-key USERNAME KEYNAME"

  @helper.get_required_arg!(@options, cmd_args, @usage, :username, "USERNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :key_name, "KEYNAME", 2)

  begin
    @helper.delete_rest("/users/#{@options.username}/keys/#{@options.key_name}")
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find key #{@options.key_name} for user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
    end
  end
end

add_command_under_category "delete-client-key", "key-rotation", "Delete a key", 2 do
  cmd_args = ARGV[3..-1]
  @helper = KeyCtlHelper.new
  @options = OpenStruct.new
  @usage = "Usage: chef-server-ctl delete-client-key ORGNAME CLIENTNAME KEYNAME"

  @helper.get_required_arg!(@options, cmd_args, @usage, :orgname, "ORGNAME", 1)
  @helper.get_required_arg!(@options, cmd_args, @usage, :clientname, "CLIENTNAME", 2)
  @helper.get_required_arg!(@options, cmd_args, @usage, :key_name, "KEYNAME", 3)

  begin
    @helper.delete_rest("/organizations/#{@options.orgname}/clients/#{@options.clientname}/keys/#{@options.key_name}")
  rescue Net::HTTPServerException => e
    if e.response.code == "404"
      @helper.exit_failure("Error: Could not find key #{@options.key_name} for user #{@options.username}.")
    else
      @helper.exit_http_fail(e)
      end
  end
end
