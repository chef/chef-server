name "chef-ha-drbd"
maintainer        "Chef, Inc."
maintainer_email  "cookbooks@chef.io"
license           "Apache 2.0"
description       "Sets up HA DRDB for the chef server"
long_description  "Sets up HA DRDB for the chef server"
version           "0.1.0"

%w{ ubuntu debian redhat centos oracle scientific fedora amazon }.each do |os|
  supports os
end
