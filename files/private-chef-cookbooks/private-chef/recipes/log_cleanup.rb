#
# Author:: Lamont Granquist (<lamont@opscode.com>)
# Author:: Seth Chisamore (<schisamo@opscode.com>)
# Copyright:: Copyright (c) 2012-2013 Opscode, Inc.
#
# All Rights Reserved
#

package "logrotate"

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
