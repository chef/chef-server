#
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

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
