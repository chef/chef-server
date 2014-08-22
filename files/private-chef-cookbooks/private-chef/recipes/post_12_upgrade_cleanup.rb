#
# Author:: Stephen Delano (<stpehen@getchef.com.com>)
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
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
  directories ["/var/opt/opscode/opscode-solr",
               "/var/log/opscode/opscode-solr"]
  files ["/etc/opscode/logrotate.d/opscode-solr"]
end
