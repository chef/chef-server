#Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/osc_upgrade"

add_command "upgrade", "Upgrade your private chef installation.", 1 do

  # Detect if OSC is present - if not, then skip to and continue with EC upgrade
  # Ask user if they want to upgrade
  if File.directory?("/opt/chef-server")
    # Do we want to refer to it as the open source chef server, since the new
    # server is open source too, even if it is based on enterprise chef?
    puts "Open Source Chef server detected."

    puts "Would you like to upgrade? [Yn]"
    answer = STDIN.gets.chomp
    if answer == 'Y' || answer == 'y'
      puts "Upgrading the Open Source Chef server."
      run_osc_upgrade
    else
      puts "Aborting upgrade, because you told me to or I don't understand the input."
      puts "You answered #{answer}"
      exit 0 # What do we want to do if the user says no?
    end
  end

  # Original EC add_command
  # Run this in all cases
  reconfigure(false)
  Dir.chdir(File.join(base_path, "embedded", "service", "partybus"))
  bundle = File.join(base_path, "embedded", "bin", "bundle")
  status = run_command("echo 'Sleeping for 2 minutes before migration' ; sleep 120 ; #{bundle} exec ./bin/partybus upgrade")
  status = run_command("#{bundle} exec ./bin/partybus upgrade")
  if status.success?
    puts "Chef Server Upgraded!"
    exit 0
  else
    exit 1
  end
end
