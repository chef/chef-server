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
knife_cmd    = ::ChefServerCtl::Config.knife_bin
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
    escaped_args = transformed_args.map { |a| Shellwords.escape(a) }.join(" ")
    # Use native knife instead of knife-opc
    full_command = "#{knife_cmd} #{opc_noun} #{opc_cmd} #{escaped_args} -c #{knife_config} -VVV"
    puts "DEBUG: Running command: #{full_command}"
    # Special handling: for user-create capture key output and write to file
    if cmd == "user-create"
      require 'mixlib/shellout'
      shell = Mixlib::ShellOut.new(full_command)
      shell.run_command
      puts shell.stdout
      # extract file path from args
      if (idx = transformed_args.index('--file')) && transformed_args[idx+1]
        keyfile = transformed_args[idx+1]
        if (key = shell.stdout[/-----BEGIN RSA PRIVATE KEY-----.*?-----END RSA PRIVATE KEY-----/m])
          File.write(keyfile, key)
          puts "Wrote private key to #{keyfile}"
        end
      end
      exit shell.exitstatus
    else
      status = run_command(full_command)
      exit status.exitstatus
    end
  end
end

# Transform arguments from knife-opc format to native knife format
def transform_knife_opc_args(args, chef_server_ctl_cmd, knife_noun, knife_verb)
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
      
      # Start with username
      transformed = [username]
      
      # Add required flags
      transformed << "--email" << email
      transformed << "--password" << password
      transformed << "--first-name" << first_name
      transformed << "--last-name" << last_name
      
      # Handle any additional flags (like --filename/--file)
      remaining_args = args[non_flag_args.length..-1] || []
      remaining_args.each do |arg|
        case arg
        when "--filename"
          transformed << "--file"
        when /^--filename=(.+)$/
          transformed << "--file=#{$1}"
        else
          transformed << arg
        end
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
