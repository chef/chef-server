#
# Author:: Stephen Delano (<stpehen@chef.io.com>)
# Copyright:: 2014-2018 Chef Software, Inc.
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

# Following an upgrade to Chef Server 12, some old data will remain
# behind.  This recipe removes all that stuff, following the upgrade,
# only when the user is satisfied with the state of their system.

private_chef_package_cleaner "opscode-webui" do
  directories ["/var/opt/opscode/opscode-webui",
               "/var/log/opscode/opscode-webui",
               "/opt/opscode/embedded/service/opscode-webui"]
end

private_chef_package_cleaner "opscode-solr" do
  directories ["/var/opt/opscode/opscode-solr",                  # solr app binaries here
               node['private_chef']['opscode-solr']['data_dir'], # solr data here
               "/var/log/opscode/opscode-solr"]
  files ["/etc/opscode/logrotate.d/opscode-solr"]
end

private_chef_package_cleaner "couchdb" do
  directories [node['private_chef']['couchdb']['data_dir'],
               "/var/log/opscode/couchdb"]
  files ["/etc/cron.d/couchdb_compact",
         "/etc/cron.d/couchdb_bounce",
         "/etc/cron.d/couchdb_compact_major_offenders",
         "/etc/opscode/logrotate.d/couchdb"]
end

private_chef_package_cleaner "opscode-account" do
  directories ["/opt/opscode/embedded/service/opscode-account",
               "/var/log/opscode/opscode-account",
               "/var/opt/opscode/opscode-account"]
end

private_chef_package_cleaner "opscode-org-creator" do
  directories ["/opt/opscode/embedded/service/opscode-org-creator",
               "/var/log/opscode/opscode-org-creator",
               "/var/opt/opscode/opscode-org-creator"]
end

private_chef_package_cleaner "opscode-certificate" do
  directories ["/opt/opscode/embedded/service/opscode-certificate",
               "/var/log/opscode/opscode-certificate",
               "/var/opt/opscode/opscode-certificate"]
end

private_chef_package_cleaner "orgmapper" do
  files ["/etc/opscode/orgmapper.conf"]
end
