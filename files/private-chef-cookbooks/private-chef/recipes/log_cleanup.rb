#
# Author:: Lamont Granquist (<lamont@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

commands = []

node['private_chef']['log_retention'].each do |service, days|
  next unless node['private_chef'][service]['enable']
  log_directory = node['private_chef'][service]['log_directory']
  commands << "find #{log_directory} -type f -mtime +#{days} -print0 1>&2 | xargs -0 rm -f"
end

cron_command = commands.join(";")

template "/etc/cron.d/opc_log_cleanup" do 
  source "opc_log_cleanup.cron"
  mode "0600"
  owner "root"
  group "root"
  variables({ :cron_command => cron_command })
end

