#
# Author:: Lamont Granquist (<lamont@chef.io>)
# Author:: Seth Chisamore (<schisamo@chef.io>)
# Copyright:: Chef Software, Inc.
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

# not using /etc/logrotate.d on purpose here so that busted system logrotation will not break
# the privatechef logrotation.
template "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/logrotate.conf" do
  source 'logrotate-opscode.conf'
  mode   '0644'
  owner  'root'
  group  'root'
end

template '/etc/cron.hourly/opc_logrotate' do
  source 'opc_logrotate.cron'
  mode   '0755'
  owner  'root'
  group  'root'
end
