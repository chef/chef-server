#Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
#
# All Rights Reserved
#
require "chef-config/dist"
require "chef"

add_command_under_category "gather-logs", "general", "Create a tarball of recent logs and system information for #{Chef::Dist::SERVER_PRODUCT} Support", 2 do
  if Process.uid != 0
    STDERR.puts "private-#{ChefConfig::Dist::SHORT}-ctl gather-logs should be run as root."
    exit 1
  end
  run_command("/opt/opscode/bin/gather-logs")
end
