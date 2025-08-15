#
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
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

require "shellwords"
require "chef-utils"
require "mixlib/cli"

knife_config = ::ChefServerCtl::Config.knife_config_file
cmd_args     = ARGV[1..-1]

# Determine knife binary path using precedence:
# 1) CSC_KNIFE_BIN environment variable (for overriding in development/testing)
# 2) Result of `which knife` command if available
# 3) Default Chef Server knife path
def resolve_knife_bin
  # Check environment variable first (must be non-empty)
  return ENV["CSC_KNIFE_BIN"] if ENV["CSC_KNIFE_BIN"]&.!empty?
  
  # Try to find knife in PATH
  which_result = `which knife 2>/dev/null`.strip
  return which_result unless which_result.empty?
  
  # Fall back to default
  "/opt/opscode/embedded/bin/knife"
end

knife_cmd = resolve_knife_bin

# Argument parser using Mixlib::CLI to separate flags from positional args
class KnifeArgumentParser
  include Mixlib::CLI
  
  # Define options that knife user create supports
  option :file,
    short: "-f FILE",
    long: "--file FILE",
    description: "Write the private key to a file"
    
  option :filename,
    long: "--filename FILE", 
    description: "Write the private key to a file (knife-opc compatibility)"
    
  option :user_key,
    long: "--user-key FILENAME",
    description: "Set the initial default key for the user from a file"
    
  option :prevent_keygen,
    short: "-k",
    long: "--prevent-keygen",
    description: "Prevent server from generating a default key pair",
    boolean: true
    
  option :orgname,
    long: "--orgname ORGNAME",
    short: "-o ORGNAME", 
    description: "Associate new user to an organization"
    
  option :passwordprompt,
    long: "--prompt-for-password",
    short: "-p",
    description: "Prompt for user password",
    boolean: true
    
  option :first_name,
    long: "--first-name FIRST_NAME",
    description: "First name for the user"
    
  option :last_name,
    long: "--last-name LAST_NAME", 
    description: "Last name for the user"
    
  option :email,
    long: "--email EMAIL",
    description: "Email for the user"
    
  option :password,
    long: "--password PASSWORD",
    description: "Password for the user"
    
  # Parse arguments and return separated positional args and config
  def self.parse_args(args)
    parser = new
    # parse_options returns the positional arguments, config gets flags
    name_args = parser.parse_options(args.dup)
    { positional: name_args, config: parser.config }
  end
end

cmds = {
  "org-create"      => ["create", "org", "Create an organization in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "org-delete"      => ["delete", "org", "Delete an organization in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "org-list"        => ["list", "org", "List all organizations in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "org-show"        => ["show", "org", "Show an organization in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "org-user-add"    => ["add", "org user", "Associate a user with an organization."],
  "org-user-remove" => ["remove", "org user", "Dissociate a user with an organization."],
  "user-create"     => ["create", "user", "Create a user in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "user-delete"     => ["delete", "user", "Delete a user in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "user-edit"       => ["edit", "user", "Edit a user in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "user-list"       => ["list", "user", "List all users in the #{ChefUtils::Dist::Server::PRODUCT}."],
  "user-show"       => ["show", "user", "Show a user in the #{ChefUtils::Dist::Server::PRODUCT}."],
}

cmds.each do |cmd, args|
  opc_cmd = args[0]
  opc_noun = args[1]
  description = args[2]
  add_command_under_category cmd, "organization-and-user-management", description, 2 do
    # Transform knife-opc arguments to knife format
    transformed_args = transform_knife_opc_args(cmd_args, cmd, opc_noun, opc_cmd)
    
    server_url = get_server_url()
    
    # Build authentication arguments
    auth_args = []
    # auth_args << "--server-url" << server_url
    auth_args << "-c" << knife_config
    # auth_args << "--config-option" << "ssl_verify_mode=verify_none"
    
    # Build command args - don't escape config options with = signs
    all_args = transformed_args + auth_args
    escaped_args = all_args.map do |arg|
      # Don't escape arguments that contain = (like config options)
      if arg.include?('=')
        arg
      else
        Shellwords.escape(arg)
      end
    end.join(" ")
    
    knife_command = "#{knife_cmd} #{opc_noun} #{opc_cmd} #{escaped_args}"
    status = run_command(knife_command)
    exit status.exitstatus
  end
end

# Transform arguments from knife-opc format to native knife format
def transform_knife_opc_args(args, chef_server_ctl_cmd, _knife_noun, _knife_verb)
  transformed = args.dup
  
  case chef_server_ctl_cmd
  when "user-create"
    # Handle knife-opc formats (based on official knife-opc banner):
    # Format 1: USERNAME FIRST_NAME LAST_NAME EMAIL PASSWORD --filename FILE
    # Format 2: USERNAME FIRST_NAME [MIDDLE_NAME] LAST_NAME EMAIL PASSWORD --filename FILE  
    # native knife format: USERNAME --email EMAIL --password PASSWORD --first-name FIRST --last-name LAST --file FILE
    
    # Use Mixlib::CLI to properly parse arguments (handles mixed flag/positional scenarios)
    parsed = KnifeArgumentParser.parse_args(args)
    positional_args = parsed[:positional]
    config = parsed[:config]
    
    # Need at least 5 positional args for knife-opc format
    if positional_args.length >= 5
      username = positional_args[0]
      
      if positional_args.length == 5
        # Format 1: USERNAME FIRST_NAME LAST_NAME EMAIL PASSWORD
        first_name = positional_args[1]
        last_name = positional_args[2]
        email = positional_args[3]
        password = positional_args[4]
      elsif positional_args.length == 6
        # Format 2: USERNAME FIRST_NAME MIDDLE_NAME LAST_NAME EMAIL PASSWORD  
        # Drop middle name - native knife doesn't support it
        first_name = positional_args[1]
        # positional_args[2] is middle_name - ignored
        last_name = positional_args[3]    
        email = positional_args[4]
        password = positional_args[5]
      else
        # Too many positional args - fallback to original
        return transform_flags_only(args)
      end
      
      # Build new argument list in modern knife format
      transformed = [username]
      transformed << "--email" << email
      transformed << "--password" << password
      transformed << "--first-name" << first_name
      transformed << "--last-name" << last_name
      
      # Add parsed flags from config, converting --filename to -f
      config.each do |key, value|
        case key
        when :filename
          transformed << "-f" << value if value
        when :file
          transformed << "-f" << value if value
        when :orgname
          transformed << "--orgname" << value if value
        when :user_key
          transformed << "--user-key" << value if value
        when :prevent_keygen
          transformed << "--prevent-keygen" if value
        when :passwordprompt
          transformed << "--prompt-for-password" if value
        # Skip :first_name, :last_name, :email, :password - we handle these above
        end
      end
    else
      # Not enough positional args for knife-opc format - just convert flags
      transformed = transform_flags_only(args)
    end
    
  when "user-list"
    # Handle --all-info option (not supported in native knife)
    if transformed.include?("--all-info") || transformed.include?("-a")
      transformed = transformed.reject { |arg| %w[--all-info -a].include?(arg) }
    end
  end
  
  transformed
end

# Transform flags only (for non-opc format args) 
def transform_flags_only(args)
  args.map do |arg|
    case arg
    when "--filename"
      "-f"
    else
      arg
    end
  end
end

# Get the Chef Server URL for knife commands
def get_server_url()
  # Get server URL - use lb_url from ChefServerCtl::Config
  begin
    server_url = ::ChefServerCtl::Config.lb_url
  rescue => e
    server_url = "https://localhost"
  end
  
  server_url
end
