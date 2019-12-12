#
# Author:: Christopher Maier (<cm@chef.io>)
# Copyright:: 2013-2018 Chef Software, Inc.
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

# Following an upgrade to Private Chef 11, some old data will remain
# behind.  This recipe removes all that stuff, following the upgrade,
# only when the user is satisfied with the state of their system.

private_chef_package_cleaner 'opscode-authz' do
  # Note that the data directory can be different depending on whether
  # you're on a DRBD-enabled system or not.
  directories ['/var/opt/opscode/opscode-authz',
               '/var/log/opscode/opscode-authz',
               '/opt/opscode/embedded/service/opscode-authz',
               "#{node['private_chef']['couchdb']['data_dir']}/db/.authorization_design",
               '/var/opt/opscode/upgrades/oc_authz_migrator']
  files ["#{node['private_chef']['couchdb']['data_dir']}/db/authorization.couch",
         "#{node['private_chef']['couchdb']['data_dir']}/db/authorization_design_documents.couch"]
end

private_chef_package_cleaner 'nagios' do
  directories ['/opt/opscode/embedded/nagios',
               '/var/opt/opscode/nagios',
               '/var/log/opscode/nagios']
  users %w(opscode-nagios
           opscode-nagios-cmd)
  groups %w(opscode-nagios
            opscode-nagios-cmd)
  files ['/var/log/opscode/nginx/nagios.access.log',
         '/var/log/opscode/nginx/nagios.error.log',
         '/etc/opscode/logrotate.d/nagios']
end

private_chef_package_cleaner 'nrpe' do
  directories ['/var/opt/opscode/nrpe',
               '/var/log/opscode/nrpe']
end

private_chef_package_cleaner 'fastcgi' do
  # There isn't really a "service", per se for this
  is_service false
  files ['/opt/opscode/embedded/conf/fastcgi.conf',
         '/var/opt/opscode/nginx/etc/fastcgi.conf']
end

private_chef_package_cleaner 'fcgiwrap' do
  directories ['/var/log/opscode/fcgiwrap']
end

private_chef_package_cleaner 'php-fpm' do
  directories ['/var/log/opscode/php-fpm']
end

private_chef_package_cleaner 'opscode-chef' do
  directories ['/var/opt/opscode/opscode-chef',
               '/var/log/opscode/opscode-chef',
               '/opt/opscode/embedded/service/opscode-chef']
end

private_chef_package_cleaner 'redis' do
  directories ['/var/opt/opscode/redis',
               '/var/log/opscode/redis']
end

private_chef_package_cleaner 'varnish'
