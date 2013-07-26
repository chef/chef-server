#
# Author:: Christopher Maier (<cm@opscode.com>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved
#
# Following an upgrade to Private Chef 11, some old data will remain
# behind.  This recipe removes all that stuff, following the upgrade,
# only when the user is satisfied with the state of their system.

private_chef_package_cleaner "opscode-authz" do
  directories ["/var/opt/opscode/opscode-authz",
               "/var/log/opscode/opscode-authz",
               "/opt/opscode/embedded/service/opscode-authz",
               "/var/opt/opscode/couchdb/db/.authorization_design"]
  files ["/var/opt/opscode/couchdb/db/authorization.couch",
         "/var/opt/opscode/couchdb/db/authorization_design_documents.couch"]
end
