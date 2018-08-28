#
# Cookbook:: chef-server-deploy
# Attributes:: default
#
# Copyright:: Copyright 2017-2018 Chef Software, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# Domain/org names
default['chef-server-deploy']['automate_server_fqdn'] = 'automate.chef.fake'
default['chef-server-deploy']['chef_server_fqdn'] = 'chef.chef.fake'
default['chef-server-deploy']['supermarket_fqdn'] = 'supermarket.chef.fake'
default['chef-server-deploy']['delivery_chef_org'] = 'delivery'

# Key locations
default['chef-server-deploy']['chef_cert_filename'] = 'wildcard.chef.co.crt'
default['chef-server-deploy']['chef_key_filename'] = 'wildcard.chef.co.key'

# Automatic node run data collection (token randomly generated with `SecureRandom.hex(32)`)
default['chef-server-deploy']['data_collection_token'] = 'e120f9ed31db404889bf0f40d83673fddf0d07d1b906643717675733ae56ea55'

# SAML (on chef server)
default['chef-server-deploy']['enable_saml'] = true

# Automate Liveness Agent
default['chef-server-deploy']['enable_liveness_agent'] = true
