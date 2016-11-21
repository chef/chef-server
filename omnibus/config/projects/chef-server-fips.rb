#
# Copyright 2015 Chef Software, Inc.
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
chef_server_contents = IO.read(File.expand_path('../chef-server.rb', __FILE__))
self.instance_eval(chef_server_contents)

name "chef-server-fips"
package_name "chef-server-fips-core"

override :fips, enabled: true

# Use chef's scripts for everything.
resources_path "#{resources_path}/../chef-server"
package_scripts_path "#{package_scripts_path}/../chef-server"
