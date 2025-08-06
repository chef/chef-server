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
knife_cmd    = "knife"
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
    
    # Use native knife
    full_command = "#{knife_cmd} #{opc_noun} #{opc_cmd} #{escaped_args}"
    
    # Special handling: for user-create capture key output and write to file
    if cmd == "user-create"
      require 'mixlib/shellout'
      
      # Extract keyfile path for cleanup tracking (only if -f was specified)
      keyfile = nil
      if (idx = transformed_args.index('-f')) && transformed_args[idx+1]
        keyfile = transformed_args[idx+1]
      end
      
      begin
        # Original: full_command_with_sudo = "sudo #{full_command}"
        # Try multiple knife paths: /opt/opscode/bin/knife || /opt/opscode/embedded/bin/knife || /usr/bin/knife || /usr/local/bin/knife
        full_command_with_sudo = "sudo #{full_command.sub('knife', '/opt/opscode/bin/knife')} || sudo #{full_command.sub('knife', '/opt/opscode/embedded/bin/knife')} || sudo #{full_command.sub('knife', '/usr/bin/knife')} || sudo #{full_command.sub('knife', '/usr/local/bin/knife')}"
        
        shell = Mixlib::ShellOut.new(full_command_with_sudo)
        shell.run_command
        
        # Show command output to user (without DEBUG prefix)
        print shell.stdout if shell.stdout && !shell.stdout.empty?
        print shell.stderr if shell.stderr && !shell.stderr.empty?
        
        # Only proceed with file operations if knife succeeded
        unless shell.error?
          # Check if knife wrote the file and fix ownership
          if keyfile && File.exist?(keyfile)
            
            # Fix file ownership - change from root to original user
            original_user = ENV['SUDO_USER']
            if original_user
              # change ownership of #{keyfile} to #{original_user}"
              chown_result = system("chown #{original_user}:#{original_user} #{keyfile}")
            end
          end
        end
        
        exit(shell.exitstatus || 0)
        
      rescue Interrupt
        # Clean up any partial files
        if keyfile && File.exist?(keyfile)
          File.delete(keyfile) rescue nil
        end
        exit(130) # Standard exit code for SIGINT
        
      rescue => e
        # Clean up any partial files
        if keyfile && File.exist?(keyfile)
          File.delete(keyfile) rescue nil
        end
        exit(1)
      end
    else
      # Original: status = run_command(full_command)
      # Try multiple knife paths: /opt/opscode/bin/knife || /opt/opscode/embedded/bin/knife || /usr/bin/knife || /usr/local/bin/knife
      multi_path_command = "#{full_command.sub('knife', '/opt/opscode/bin/knife')} || #{full_command.sub('knife', '/opt/opscode/embedded/bin/knife')} || #{full_command.sub('knife', '/usr/bin/knife')} || #{full_command.sub('knife', '/usr/local/bin/knife')}"
      status = run_command(multi_path_command)
      exit status.exitstatus
    end
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
        # Drop middle name - native knife doesn't support it
        first_name = args[1]
        # args[2] is middle_name - ignored
        last_name = args[3]    
        email = args[4]
        password = args[5]
      else
        # Fallback to original args if format doesn't match
        return transformed
      end
      
      # Start with username (pass through as-is, let user control quoting)
      transformed = [username]
      
      # Add all supported fields - native knife supports first/last name
      transformed << "--email" << email
      transformed << "--password" << password
      transformed << "--first-name" << first_name
      transformed << "--last-name" << last_name
      
      # Handle any additional flags (like --filename/-f)
      remaining_args = args[non_flag_args.length..-1] || []
      remaining_args.each do |arg|
        case arg
        when "--filename"
          transformed << "-f"
        else
          # Pass through all other flags as-is (including -f)
          transformed << arg
        end
      end
      
      # DO NOT auto-add -f - let knife output to STDOUT if no file specified
      # This matches chef-server-ctl official behavior: PEM to STDOUT unless -f/--filename provided
    else
      # Handle --filename to -f conversion for other formats
      transformed = args.map do |arg|
        case arg
        when "--filename"
          "-f"
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
  rescue => e
    server_url = "https://localhost"
  end
  
  server_url
end
