#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command "upgrade", "Upgrade your private chef installation.", 1 do
  reconfigure(false)
  status = run_command("cd /opt/opscode/embedded/service/partybus && /opt/opscode/embedded/bin/ruby bin/partybus upgrade")
  if status.success?
    puts "Chef Server Upgraded!"
    exit 0
  else
    exit 1
  end
end
