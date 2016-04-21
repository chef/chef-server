#
# Author:: Tyler Cloke (<tyler@chef.io>)
# Copyright:: Copyright (c) 2014, Chef Software, Inc.
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

# Generate knife config that chef-server-ctl commands
# that wrap knife-opc use to make knife calls.

vip_root = node['private_chef']['lb']['vip']
if node['private_chef']['nginx']['enable_non_ssl']
  port = node['private_chef']['nginx']['non_ssl_port']
  prefix = "http"
else
  port = node['private_chef']['nginx']['ssl_port']
  prefix = "https"
end
vip = "#{vip_root}:#{port}"

template "/etc/opscode/pivotal.rb" do
  source "pivotal.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(:vip => vip, :prefix => prefix)
end
