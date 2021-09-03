#
# Copyright:: Copyright (c) 2012-2019 Chef Software, Inc.
#
# All Rights Reserved
#

add_command_under_category "master-recover", "high-availability", "Set this server to HA master state, ignoring VRRP.", 1 do
  puts err_CONFIG_DRBD_HA_REMOVED("master-recover")
  exit 1
end

add_command_under_category "backup-recover", "high-availability", "Set this server to HA backup state, ignoring VRRP", 1 do
  puts err_CONFIG_DRBD_HA_REMOVED("backup-recover")
  exit 1
end

#
# ha-status
#  - check keepalived is actually enabled in the config
#  - check that the current state matches requested state of master or backup
#  - check that the VRRP IP address state is correct based on our state
#  - run a custom script to check any custom block device/vip state
#  - check that the runit service status is correct based on our state
#

add_command_under_category "ha-status", "high-availability", "Show the status of high availability services.", 1 do
  puts err_CONFIG_DRBD_HA_REMOVED("ha-status")
  exit 1
end

def err_CONFIG_DRBD_HA_REMOVED(command)
  <<EOM
DRBD_HA_001: HA Command #{command} no longer supported.

The DRBD/keepalived based HA subsystem was deprecated as of Chef Server
12.9, and officially reached end of life on 2019-03-31. It has been
disabled in Chef Server 13.

See this post for more details:
https://blog.chef.io/2018/10/02/end-of-life-announcement-for-drbd-based-ha-support-in-chef-server/

What are my options?

Chef Backend was announced over two years ago and is the recommended solution
for all customers. It is a licensed product and available under the terms
of a Chef Infra License.

For more information on migrating from DRBD HA to Chef Backend or other HA, see this blog
post and webinar: Best Practices for Migrating your Chef Server at
https://blog.chef.io/2018/04/06/best-practices-for-migrating-your-chef-server/

Customers in cloud environments are also encouraged to look at AWS OpsWorks
and the Chef Automate Managed Service for Azure.

EOM
end
