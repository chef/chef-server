name "chef-ha-native"
maintainer        "Chef Software, Inc."
maintainer_email  "cookbooks@chef.io"
license           "Apache 2.0"
description       "Sets up UNRELEASED per-service replication for Chef Server"
long_description  "Sets up UNRELEASED per-service replication for Chef Server"
version           "0.1.0"

%w{ ubuntu debian redhat centos oracle scientific fedora amazon }.each do |os|
  supports os
end

depends "enterprise"
