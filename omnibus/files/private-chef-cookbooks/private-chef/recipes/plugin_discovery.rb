# Author:: Steven Danna <steve@chef.io>
# Copyright:: 2015-2018 Chef Software, Inc.
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

# TODO Does this go away with HA?
BACKUP_PLUGIN_LOCATIONS = %w(/opt/opscode/chef-server-plugin.rb)

node.default['available-plugins'] = EnterprisePluginCollection.from_glob("/var/opt/opscode/plugins/*.rb")

#
# Load plugins from backup locations if we didn't find anything
# in the plugin directory.
#
if node.default['available-plugins'].empty?
  c = EnterprisePluginCollection.new()
  BACKUP_PLUGIN_LOCATIONS.each do |path|
    if ::File.exist?(path)
      c.load_from_file(path)
    end
  end
  node.default['available-plugins'] = c.plugins
end
