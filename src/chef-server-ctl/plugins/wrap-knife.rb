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

knife_config = ::ChefServerCtl::Config.knife_config_file
cmd_args     = ARGV[1..-1]

# knife path
def resolve_knife_bin
  ::ChefServerCtl::Config.knife_bin
end

knife_cmd = resolve_knife_bin

# Transform arguments from knife-opc format to native knife format
def transform_knife_opc_args(args, chef_server_ctl_cmd, _knife_noun, _knife_verb)
  transformed = args.dup
  
  case chef_server_ctl_cmd
  when "user-create"
    # Handle knife-opc formats (based on official knife-opc banner):
    # Format 1: USERNAME FIRST_NAME LAST_NAME EMAIL PASSWORD --filename FILE
    # Format 2: USERNAME FIRST_NAME [MIDDLE_NAME] LAST_NAME EMAIL PASSWORD --filename FILE  
    # native knife format: USERNAME --email EMAIL --password PASSWORD --first-name FIRST --last-name LAST --file FILE
    
    # Simple argument parsing that separates flags from positional args
    positional_args = []
    flags = {}
    i = 0
    while i < args.length
      arg = args[i]
      case arg
      when "--filename", "-f"
        flags[:filename] = args[i + 1] if i + 1 < args.length
        i += 2
      when "--file"
        flags[:file] = args[i + 1] if i + 1 < args.length
        i += 2
      when "--user-key"
        flags[:user_key] = args[i + 1] if i + 1 < args.length
        i += 2
      when "--prevent-keygen", "-k"
        flags[:prevent_keygen] = true
        i += 1
      when "--orgname", "-o"
        flags[:orgname] = args[i + 1] if i + 1 < args.length
        i += 2
      when "--prompt-for-password", "-p"
        flags[:passwordprompt] = true
        i += 1
      when "--first-name"
        flags[:first_name] = args[i + 1] if i + 1 < args.length
        i += 2
      when "--last-name"
        flags[:last_name] = args[i + 1] if i + 1 < args.length
        i += 2
      when "--email"
        flags[:email] = args[i + 1] if i + 1 < args.length
        i += 2
      when "--password"
        flags[:password] = args[i + 1] if i + 1 < args.length
        i += 2
      else
        positional_args << arg
        i += 1
      end
    end
    
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
      
      # Add parsed flags, converting --filename to -f
      flags.each do |key, value|
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
