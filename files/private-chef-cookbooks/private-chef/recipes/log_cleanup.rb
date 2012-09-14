#
# Author:: Lamont Granquist (<lamont@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

commands = []

node['private_chef']['logs']['log_retention'].each do |service, days|
  next unless node['private_chef'][service]['enable'] || service == "jetty"
  log_directory = node['private_chef'][service]['log_directory']
  commands << "find #{log_directory} -type f \\( -mtime +#{days} \\! -name config \\! -name lock \\! -name state \\! -name newstate \\) -print0 2>/dev/null | xargs -0 rm -f"
end

template "/opt/opscode/bin/opc_log_cleanup" do
  source "opc_log_cleanup"
  mode   "0755"
  owner  "root"
  group  "root"
  variables({ :commands => commands })
end

template "/etc/cron.d/opc_log_cleanup" do 
  source "opc_log_cleanup.cron"
  mode "0600"
  owner "root"
  group "root"
  variables({ :cron_command => "/opt/opscode/bin/opc_log_cleanup" })
end

package "logrotate"

# not using /etc/logrotate.d on purpose here so that busted system logrotation will not break
# the privatechef logrotation.
template "/etc/opscode/logrotate.conf" do
  source "logrotate-opscode.conf"
  mode   "0644"
  owner  "root"
  group  "root"
  variables(node['private_chef']['logs'].to_hash)
end

template "/etc/cron.d/opc_logrotate.cron" do
  source "opc_logrotate.cron"
  mode   "0600"
  owner  "root"
  group  "root"
end

