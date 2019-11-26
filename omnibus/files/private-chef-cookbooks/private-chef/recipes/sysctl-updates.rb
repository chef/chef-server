
# Author:: Marc Paradise (<marc@chef.io>)
# Copyright:: 2013-2018 Chef Software, Inc.
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

# Typically speaking supported systems will default to this value
# but we'll want to ensure that it's correct. Many of our services currently rely on
# automatic dual-binding of listening ports to both ipv6 and ipv4.  This directive ensures
# the behavior.
# net.ipv6.bindv6only = 0

execute 'sysctl-reload' do
  command '/sbin/sysctl -p /etc/sysctl.conf || true'
  action :nothing
end

# Ideally, we'd just render a template into sysctl.d - but that's
# 6.2 and later under centos. This method will work for all supported
# platforms.

bash 'dual ip4/ip6 portbind' do
  user 'root'
  code <<-EOF
    echo 'net.ipv6.bindv6only = 0' >> /etc/sysctl.conf
  EOF
  notifies :run, 'execute[sysctl-reload]', :immediately
  not_if "egrep '^net\.ipv6\.bindv6only = 0' /etc/sysctl.conf"
  only_if { PrivateChef['use_ipv6'] == true }
end
