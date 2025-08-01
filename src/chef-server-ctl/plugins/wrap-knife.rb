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

# knife_config = ::ChefServerCtl::Config.knife_config_file
knife_cmd    = "knife"
puts "=== REVISION 4 - ADDING sudo TO KNIFE COMMAND ==="
puts "DEBUG: Using knife from PATH: #{knife_cmd}"
puts "KNIFE PATH: #{knife_cmd}"
cmd_args     = ARGV[1..-1]

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
    # Transform knife-opc arguments to native knife format
    puts "DEBUG: Input args: #{cmd_args.inspect}"
    transformed_args = transform_knife_opc_args(cmd_args, cmd, opc_noun, opc_cmd)
    puts "DEBUG: Transformed args: #{transformed_args.inspect}"
    
    server_url = get_server_url()
    
    # Build authentication arguments directly here
    auth_args = []
    auth_args << "--server-url" << server_url
    auth_args << "--user" << "pivotal"
    auth_args << "--key" << "/etc/opscode/pivotal.pem"
    auth_args << "--config-option" << "ssl_verify_mode=verify_none"
    
    puts "DEBUG: Auth args: #{auth_args.inspect}"
    
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
    
    # Use native knife instead of knife-opc (remove -VVV which might interfere)
    full_command = "#{knife_cmd} #{opc_noun} #{opc_cmd} #{escaped_args}"
    puts "===== EXECUTING COMMAND: #{full_command} ====="
    
    # Special handling: for user-create capture key output and write to file
    if cmd == "user-create"
      require 'mixlib/shellout'
      
      # Debug working directory
      puts "DEBUG: Current working directory: #{`pwd`.strip}"
      puts "DEBUG: HOME environment variable: #{`echo $HOME`.strip}"
      puts "DEBUG: pwd command output: #{`pwd`.strip}"
      puts "DEBUG: Directory contents BEFORE knife:"
      puts `ls -la`
      
      # Use system() instead of Mixlib::ShellOut to match manual execution
      puts "DEBUG: Executing with system() instead of Mixlib::ShellOut"
      puts "DEBUG: About to execute: #{full_command}"
      
      # Try using exec form to avoid shell interpretation issues
      args_array = ["sudo", knife_cmd, opc_noun, opc_cmd] + all_args
      puts "DEBUG: Exec array form: #{args_array.inspect}"
      
      # Use spawn with explicit arguments instead of shell string
      pid = spawn(*args_array)
      _, status = Process.wait2(pid)
      puts "DEBUG: spawn() exit status: #{status.exitstatus}"
      puts "DEBUG: spawn() success?: #{status.success?}"
      
      puts "DEBUG: Directory contents AFTER knife:"
      puts `ls -la`
      
      # extract file path from args
      if (idx = transformed_args.index('--file')) && transformed_args[idx+1]
        keyfile = transformed_args[idx+1]
        puts "DEBUG: Looking for private key file: #{keyfile}"
        
        # Check if knife wrote the file directly (which is what --file does)
        if File.exist?(keyfile)
          puts "DEBUG: SUCCESS! Private key file was created: #{keyfile}"
          puts "DEBUG: File size: #{File.size(keyfile)} bytes"
        else
          puts "DEBUG: ERROR! Private key file was not created: #{keyfile}"
        end
      else
        puts "DEBUG: No --file argument found in transformed args"
      end
      exit(status.exitstatus || 0)
    else
      status = run_command(full_command)
      exit status.exitstatus
    end
  end
end

# Transform arguments from knife-opc format to native knife format
def transform_knife_opc_args(args, chef_server_ctl_cmd, _knife_noun, _knife_verb)
  transformed = args.dup
  
  case chef_server_ctl_cmd
  when "user-create"
    # Handle knife-opc formats:
    # Format 1: USERNAME FIRST_NAME LAST_NAME EMAIL PASSWORD --filename FILE
    # Format 2: USERNAME FIRST_NAME MIDDLE_NAME LAST_NAME EMAIL PASSWORD --filename FILE  
    # native knife format: USERNAME --email EMAIL --password PASSWORD --first-name FIRST --last-name LAST --file FILE
    
    if args.length >= 5 && !args[0].start_with?('-')
      username = args[0]
      
      # Detect format based on number of args before flags
      non_flag_args = args.take_while { |arg| !arg.start_with?('-') }
      
      if non_flag_args.length == 5
        # Format 1: USERNAME FIRST_NAME LAST_NAME EMAIL PASSWORD
        first_name = args[1]
        last_name = args[2]
        email = args[3]
        password = args[4]
      elsif non_flag_args.length == 6
        # Format 2: USERNAME FIRST_NAME MIDDLE_NAME LAST_NAME EMAIL PASSWORD  
        first_name = args[1]
        middle_name = args[2]  # Optional middle name, combine with first
        last_name = args[3]    
        email = args[4]
        password = args[5]
        # Combine first and middle names
        first_name = "#{first_name} #{middle_name}"
      else
        # Fallback to original args if format doesn't match
        return transformed
      end
      
      # Start with username (pass through as-is, let user control quoting)
      transformed = [username]
      
      # Use minimal working syntax (like colleague's successful commands)
      # Don't include --first-name/--last-name as they may not be supported
      transformed << "--email" << email
      transformed << "--password" << password
      # transformed << "--first-name" << first_name
      # transformed << "--last-name" << last_name
      
      # Handle any additional flags (like --filename/--file)
      remaining_args = args[non_flag_args.length..-1] || []
      file_specified = false
      remaining_args.each do |arg|
        case arg
        when "--filename"
          transformed << "--file"
          file_specified = true
        when /^--filename=(.+)$/
          transformed << "--file=#{$1}"
          file_specified = true
        when "--file"
          transformed << arg
          file_specified = true
        when /^--file=(.+)$/
          transformed << arg
          file_specified = true
        else
          transformed << arg
        end
      end
      
      # Always add --file for private key generation if not specified
      unless file_specified
        transformed << "--file" << "#{username}.pem"
      end
    else
      # Handle --filename to --file conversion for other formats
      transformed = args.map do |arg|
        case arg
        when "--filename"
          "--file"
        when /^--filename=(.+)$/
          "--file=#{$1}"
        else
          arg
        end
      end
    end
    
  when "user-list"
    # Handle --all-info option (not supported in native knife)
    # TODO: Revisit when suitable native knife option is found
    if transformed.include?("--all-info") || transformed.include?("-a")
      transformed = transformed.reject { |arg| %w[--all-info -a].include?(arg) }
    end
  end
  
  transformed
end

# Get the Chef Server URL for knife commands
def get_server_url()
  # Get server URL - use lb_url from ChefServerCtl::Config
  begin
    server_url = ::ChefServerCtl::Config.lb_url
    puts "DEBUG: Using server URL from config: #{server_url}"
  rescue => e
    puts "DEBUG: Failed to get lb_url from config: #{e.message}"
    server_url = "https://localhost"
    puts "DEBUG: Falling back to server URL: #{server_url}"
  end
  
  server_url
end
