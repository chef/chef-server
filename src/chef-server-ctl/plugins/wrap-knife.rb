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
knife_cmd    = "knife"
cmd_args     = ARGV[1..-1]

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
      
      # DEBUG: Show what we're about to execute
      puts "DEBUG: knife_config = #{knife_config}"
      puts "DEBUG: keyfile = #{keyfile}"
      puts "DEBUG: transformed_args = #{transformed_args.inspect}"
      puts "DEBUG: auth_args = #{auth_args.inspect}"
      puts "DEBUG: full_command = #{full_command}"
      
      # DEBUG: Check if config file exists and show contents
      if File.exist?(knife_config)
        puts "DEBUG: Config file exists at #{knife_config}"
        # puts "DEBUG: Config file contents:"
        # puts File.read(knife_config)
      else
        puts "DEBUG: Config file does not exist at #{knife_config}"
      end
      
      begin
        # Original approach but with debugging
        # Try multiple knife paths: /opt/opscode/bin/knife || /opt/opscode/embedded/bin/knife || /usr/bin/knife || /usr/local/bin/knife
        full_command_with_sudo = "sudo #{full_command.sub('knife', '/opt/opscode/bin/knife')} || sudo #{full_command.sub('knife', '/opt/opscode/embedded/bin/knife')} || sudo #{full_command.sub('knife', '/usr/bin/knife')} || sudo #{full_command.sub('knife', '/usr/local/bin/knife')}"
        puts "DEBUG: Executing: #{full_command_with_sudo}"
        
        shell = Mixlib::ShellOut.new(full_command_with_sudo)
        shell.run_command
        
        # DEBUG: Show command results
        puts "DEBUG: Command exitstatus = #{shell.exitstatus}"
        puts "DEBUG: Command stdout = #{shell.stdout.inspect}"
        puts "DEBUG: Command stderr = #{shell.stderr.inspect}"
        
        # Show command output to user (without DEBUG prefix)
        print shell.stdout if shell.stdout && !shell.stdout.empty?
        print shell.stderr if shell.stderr && !shell.stderr.empty?
        
        # Only proceed with file operations if knife succeeded
        unless shell.error?
          # DEBUG: Check if keyfile was supposed to be created
          if keyfile
            puts "DEBUG: Looking for keyfile at #{keyfile}"
            if File.exist?(keyfile)
              puts "DEBUG: Keyfile found, fixing ownership"
              # Fix file ownership - change from root to original user
              original_user = ENV['SUDO_USER']
              if original_user
                # change ownership of #{keyfile} to #{original_user}"
                chown_result = system("chown #{original_user}:#{original_user} #{keyfile}")
                puts "DEBUG: chown result = #{chown_result}"
              else
                puts "DEBUG: No SUDO_USER found, skipping chown"
              end
            else
              puts "DEBUG: Keyfile not found at expected location"
              
              # DEBUG: Test knife directly without shell escaping
              puts "DEBUG: Testing knife command manually..."
              keyfile_dir = File.dirname(keyfile)
              simple_command = "/opt/opscode/bin/knife user create testmanual --email testmanual@example.com --password test123 --first-name Test --last-name Manual -f #{keyfile_dir}/testmanual.pem -c #{knife_config}"
              puts "DEBUG: Manual command: #{simple_command}"
              manual_result = system(simple_command)
              puts "DEBUG: Manual knife result: #{manual_result}"
              if File.exist?("#{keyfile_dir}/testmanual.pem")
                puts "DEBUG: Manual knife created file successfully!"
              else
                puts "DEBUG: Manual knife also failed to create file"
              end
            end
          else
            puts "DEBUG: No keyfile specified (-f flag not used)"
          end
        else
          puts "DEBUG: Command failed with error"
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
    # TODO: Revisit when suitable native knife option is found
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
