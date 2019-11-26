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

plugins = node.default['available-plugins']
plugins.each do |p|
  next unless p.parent_plugin.nil?

  p.enabled true if node['private_chef']['enabled-plugins'].include?(p.name)
  p.enabled false if node['private_chef']['disabled-plugins'].include?(p.name)
end

missing_plugins = node['private_chef']['enabled-plugins'] - plugins.map(&:name)
unless missing_plugins.empty?
  raise "could not find plugins: #{missing_plugins}"
end

plugins.each do |plugin|
  next unless plugin.parent_plugin.nil?

  if plugin.cookbook_path.nil?
    Chef::Log.warn("The plugin #{plugin.name} does not include a cookbook path.")
    next
  end

  chef_run plugin.run_list do
    cookbook_path plugin.cookbook_path
    included_attrs ['private_chef']
  end
end
