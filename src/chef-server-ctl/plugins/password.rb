#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

require "highline/import"
require "shellwords"

knife_config = ::ChefServerCtl::Config.knife_config_file
knife_cmd    = "#{::ChefServerCtl::Config.knife_bin} opc user password"

add_command_under_category "password", "organization-and-user-management", "Set a user's password or System Recovery Password.", 2 do
  # changed arg to turn on ldap to --enable-external-auth since that makes more sense to new users, but left in
  # --disable for older users.
  unless ARGV.length == 2 || (ARGV.length == 3 && (ARGV[2] == "--disable" || ARGV[2] == "--enable-external-auth"))
    STDERR.puts "Usage: private-chef-ctl password <username> [--enable-external-auth]"
    exit 1
  end

  running_config
  username = ARGV[1]

  # if --enable-external-auth was not passed, we want to either simply change the password if ldap isn't
  # in use on the system, or change the password and also set the user for system recovery.
  if ARGV.length == 2
    password = HighLine.ask("Enter the new password:  " ) { |q| q.echo = "*" }
    password2 = HighLine.ask("Enter the new password again:  " ) { |q| q.echo = "*" }
    if password != password2
      STDERR.puts "Passwords did not match"
      exit 1
    end
    if password == "" && ldap_authentication_enabled?
      example_cmd = "'chef-server-ctl password #{username} --enable-external-auth'"
      STDERR.puts "You entered a blank password. If you were trying to enable ldap try #{example_cmd}?"
      exit 1
    end

    # if ldap is enabled, we just put this user in recovery mode and change his/her password
    message = if ldap_authentication_enabled?
                "User enabled for system recovery and the user's password has been changed."
              else # if ldap isn't being used, we simply changed the password
                "User's password has been updated."
              end

    # enable system recovery and update the password
    run_knife_opc_cmd("#{knife_cmd} #{Shellwords.escape(username)} #{Shellwords.escape(password)} -c #{knife_config}", message)

  else # if --enable-external-auth was passed, enable ldap for this user
    if ldap_authentication_enabled?
      run_knife_opc_cmd("#{knife_cmd} #{Shellwords.escape(username)} --enable-external-auth -c #{knife_config}", "External authentication enable for user.")
    else # doesn't make sense to pass --enable-external-auth if ldap isn't in use on the system
      STDERR.puts "External authentication (such as LDAP) must be enabled to clear a user's password."
      exit 1
    end
  end
end

# Private Script Functions
def run_knife_opc_cmd(cmd, message)
  run_command(cmd)
  if $?.exitstatus != 0
    STDERR.puts "Command failed"
    exit $?.exitstatus
  end
  puts message
end

def ldap_authentication_enabled?
  # NOTE(ssd) 2018-08-09: Currently neither of the Habitat-ized Chef
  # Servers support ldap authentication.
  if ::ChefServerCtl::Config.habitat_mode
    false
  else
    running_config["private_chef"]["ldap"] && running_config["private_chef"]["ldap"]["enabled"]
  end
end
