#Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "/opt/opscode/embedded/service/omnibus-ctl/osc_upgrade"

add_command "upgrade", "Upgrade your private chef installation.", 1 do

  # Since this is evaled, need to have methods first so they can be picked up

  def detect_osc
    if File.directory?("/opt/chef-server")
      true
    else
      false
    end
  end

  def upgrade?
    # This needs to handle a passed in flag so user input is not needed
    puts "Would you like to upgrade? [Yn]"

    answer = STDIN.gets.chomp
    if answer == 'Y' || answer == 'y'
      true
          else
      false
    end
  end

  if detect_osc
    puts "Open Source Chef 11 or older server detected."
    if upgrade?
      puts "Upgrading the Open Source Chef server."
      run_osc_upgrade
    else
      puts "Aborting upgrade."
      exit 0
    end
  end

  # Original EC upgrade path
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
