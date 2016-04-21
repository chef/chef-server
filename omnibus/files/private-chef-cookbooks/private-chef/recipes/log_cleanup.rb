#
# Author:: Lamont Granquist (<lamont@chef.io>)
# Author:: Seth Chisamore (<schisamo@chef.io>)
# Copyright:: Copyright (c) 2012-2013 Opscode, Inc.
#
# All Rights Reserved
#

# BEGIN LEGACY CRONTAB CLEANUP
#
# OPC log rotation was previously controlled by multiple full-blown crontabs.
# A simple `/etc/cron.hourly/` script now controlls the rotation so we'll
# cleanup all legacy crontabs.
#
file "/etc/cron.d/opc_log_cleanup" do
  action :delete
end

file "/etc/cron.d/opc_logrotate.cron" do
  action :delete
end
# END LEGACY CRONTAB CLEANUP

# package "logrotate"

# not using /etc/logrotate.d on purpose here so that busted system logrotation will not break
# the privatechef logrotation.
template "/etc/opscode/logrotate.conf" do
  source "logrotate-opscode.conf"
  mode   "0644"
  owner  "root"
  group  "root"
end

template "/etc/cron.hourly/opc_logrotate" do
  source "opc_logrotate.cron"
  mode   "0755"
  owner  "root"
  group  "root"
end
