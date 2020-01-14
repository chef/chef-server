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

if is_data_master?
  # Contents of the OC ID app's JSON data, to be called later
  oc_id_app = proc do
    begin
      Chef::JSONCompat.from_json(
        open('/etc/opscode/oc-id-applications/analytics.json').read
      )
    rescue Errno::ENOENT
      Chef::Log.warn('No analytics oc-id-application present. Skipping')
      {}
    end
  end

  directory '/etc/opscode-analytics' do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode '0775'
    recursive true
  end

  # Write out the config files for actions to load in order to interface with this EC
  # instance
  #
  file '/etc/opscode-analytics/webui_priv.pem' do
    owner OmnibusHelper.new(node).ownership['owner']
    group 'root'
    mode '0600'
    content lazy { ::File.open('/etc/opscode/webui_priv.pem').read }
  end

  file '/etc/opscode-analytics/actions-source.json' do
    owner 'root'
    mode '0600'
    sensitive true
    content lazy {
      Chef::JSONCompat.to_json_pretty(
        private_chef: {
          api_fqdn: node['private_chef']['lb']['api_fqdn'],
          oc_id_application: oc_id_app.call,
        }
      )
    }
  end
end
