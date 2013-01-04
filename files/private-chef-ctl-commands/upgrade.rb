#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command "upgrade", "Upgrade your private chef installation.", 1 do
  reconfigure(false)
  Dir.chdir(File.join(base_path, "embedded", "service", "partybus"))
  bundle = File.join(base_path, "embedded", "bin", "bundle")
  status = run_command("#{bundle} exec ./bin/partybus upgrade")
  if status.success?
    puts "Chef Server Upgraded!"
    exit 0
  else
    exit 1
  end
end
