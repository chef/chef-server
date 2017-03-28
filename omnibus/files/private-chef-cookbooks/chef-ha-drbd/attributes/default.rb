##
# DRBD
##
default['private_chef']['drbd']['dir'] = "/var/opt/opscode/drbd"
default['private_chef']['drbd']['data_dir'] = "/var/opt/opscode/drbd/data"
default['private_chef']['drbd']['sync_rate'] = "40M"
default['private_chef']['drbd']['device'] = "/dev/drbd0"
default['private_chef']['drbd']['disk'] = "/dev/opscode/drbd"
default['private_chef']['drbd']['notify_split_brain_sh'] = '/usr/lib/drbd/notify-split-brain.sh'
default['private_chef']['drbd']['flexible_meta_disk'] = "internal"
default['private_chef']['drbd']['primary']['fqdn'] = node['fqdn']
default['private_chef']['drbd']['primary']['ip'] = node['ipaddress']
default['private_chef']['drbd']['primary']['port'] = 7788
default['private_chef']['drbd']['secondary']['fqdn'] = node['fqdn']
default['private_chef']['drbd']['secondary']['ip'] = node['ipaddress']
default['private_chef']['drbd']['secondary']['port'] = 7788
default['private_chef']['drbd']['ipv6_on'] = PrivateChef['use_ipv6']
if File.exists?("/sbin/drbdadm")
  default['private_chef']['drbd']['version'] = `/sbin/drbdadm --version | \
    grep DRBDADM_VERSION= | cut -d "=" -f 2`.chomp!
else
  Chef::Log.debug("No DRBD version available!")
  default['private_chef']['drbd']['version'] = nil
end
