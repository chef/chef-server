#
# Author:: Stephan Renatus <srenatus@chef.io>
# Copyright:: 2016-2018 Chef Software, Inc.
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

# If the file is there, we don't care where it came from;
# otherwise, generate the keys.
#
# The reason we're not checking anything more specific here is that
# the preflight_bootstrap_validator checks ensure that either the key is
# present (that is, it was copied here), or will be generated because _this_
# is the node adding the pivotal user.

unless PrivateChef.credentials.exist?("#{Chef::Dist::Server::SHORT}", 'superuser_key')
  pivotal_key = OpenSSL::PKey::RSA.generate(2048)
  PrivateChef.credentials.add("#{Chef::Dist::Server::SHORT}", 'superuser_key',
    value: pivotal_key.to_pem,
    frozen: true)

  # TODO: 2017-02-28 mp: let's consider making this the default behavior
  # of any write to CredentialsCollection -
  PrivateChef.credentials.save
end

if !PrivateChef.credentials.exist?("#{Chef::Dist::Server::SHORT}", 'webui_key')
  webui_key = OpenSSL::PKey::RSA.generate(2048)
  PrivateChef.credentials.add("#{Chef::Dist::Server::SHORT}", 'webui_key',
    value: webui_key.to_pem,
    frozen: true)
  # Store the public key in its own key for easy access
  PrivateChef.credentials.add("#{Chef::Dist::Server::SHORT}", 'webui_pub_key',
    value: webui_key.public_key.to_s,
    frozen: true)
  PrivateChef.credentials.save
elsif !PrivateChef.credentials.exist?("#{Chef::Dist::Server::SHORT}", 'webui_pub_key')
  webui_string = PrivateChef.credentials.get("#{Chef::Dist::Server::SHORT}", 'webui_key')
  webui_key = OpenSSL::PKey::RSA.new(webui_string)
  PrivateChef.credentials.add("#{Chef::Dist::Server::SHORT}", 'webui_pub_key',
    value: webui_key.public_key.to_s,
    frozen: true)
  PrivateChef.credentials.save
end

webui_key = OpenSSL::PKey::RSA.new(PrivateChef.credentials.get("#{Chef::Dist::Server::SHORT}", 'webui_key'))

if node['private_chef']['insecure_addon_compat']
  file '/etc/opscode/pivotal.pem'  do
    owner OmnibusHelper.new(node).ownership['owner']
    group 'root'
    mode '0600'
    sensitive true
    content PrivateChef.credentials.get("#{Chef::Dist::Server::SHORT}", 'superuser_key')
  end

  file '/etc/opscode/webui_priv.pem' do
    owner OmnibusHelper.new(node).ownership['owner']
    group 'root'
    mode '0600'
    sensitive true
    content webui_key.to_pem
  end

  file '/etc/opscode/webui_pub.pem' do
    owner 'root'
    group 'root'
    mode '0644'
    sensitive true
    content webui_key.public_key.to_s unless webui_key.nil?
  end
else
  #  These keys are no longer kept directly on the FS
  #  delete them if they're present.
  %w(/etc/opscode/pivotal.pem /etc/opscode/webui_priv.pem /etc/opscode/webui_pub.pem).each do |f|
    file f do
      action :delete
      sensitive true
    end
  end
end
