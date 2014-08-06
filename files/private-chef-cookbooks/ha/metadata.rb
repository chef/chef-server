name "ha"
maintainer        "Chef, Inc."
maintainer_email  "cookbooks@opscode.com"
license           "Apache 2.0"
description       "Sets up HA for the chef server"
long_description  "Sets up HA for the chef server"
version           "0.1.0"

%w{ ubuntu debian redhat centos oracle scientific fedora amazon }.each do |os|
  supports os
end

depends 'private-chef'
