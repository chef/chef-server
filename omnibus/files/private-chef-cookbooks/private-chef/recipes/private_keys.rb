#
# Author:: Stephan Renatus <srenatus@chef.io>
# Copyright:: Copyright (c) 2016 Chef Software, Inc.
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

pivotal_key_path = "/etc/opscode/pivotal.pem"
pivotal_key = if File.exists?(pivotal_key_path)
                OpenSSL::PKey::RSA.new(File.read(pivotal_key_path))
              else
                OpenSSL::PKey::RSA.generate(2048)
              end

# Setting at the top level so that we don't export this to chef-server-running.json
node.default['bootstrap']['superuser_public_key'] = pivotal_key.public_key.to_s

file pivotal_key_path do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"
  content pivotal_key.to_pem.to_s
  sensitive true
end

webui_priv_key_path = "/etc/opscode/webui_priv.pem"
webui_key = if File.exists?(webui_priv_key_path)
                OpenSSL::PKey::RSA.new(File.read(webui_priv_key_path))
            else
                webui_key = OpenSSL::PKey::RSA.generate(2048) if new_webui
            end

  file "/etc/opscode/webui_pub.pem" do
  owner "root"
  group "root"
  mode "0644"
  content webui_key.public_key.to_s
end

file webui_priv_key_path do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"
  content webui_key.to_pem.to_s
end
