name              "private-chef"
maintainer        "Chef Software, Inc."
maintainer_email  "cookbooks@chef.io"
license           "Apache-2.0"
description       "Installs and configures Chef Server from Omnibus"
version           "0.1.1"

%w{ ubuntu debian redhat centos oracle scientific fedora amazon }.each do |os|
  supports os
end

depends          'enterprise' # grabbed via Berkshelf + Git

chef_version '>= 14.4'
