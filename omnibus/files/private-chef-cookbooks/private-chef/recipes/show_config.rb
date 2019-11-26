#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: 2012-2018 Chef Software, Inc.
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

if File.exist?('/etc/opscode/chef-server.rb')
  PrivateChef[:node] = node
  PrivateChef.from_file('/etc/opscode/chef-server.rb')
end
config = PrivateChef.generate_config(node['fqdn'])

puts Chef::JSONCompat.to_json_pretty(config)
