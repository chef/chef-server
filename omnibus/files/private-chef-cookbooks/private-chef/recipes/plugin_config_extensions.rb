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

node['available-plugins'].each do |p|
  if p.config_extension_path && ::File.exist?(p.config_extension_path)
    Chef::Log.warn("Configuration extensions not yet implemented")
    eval(::File.read(p.config_extension_path))
  end
end
