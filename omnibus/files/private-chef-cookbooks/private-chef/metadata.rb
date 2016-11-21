name "private-chef"
maintainer        "Opscode, Inc."
maintainer_email  "cookbooks@chef.io"
license           "Apache 2.0"
description       "Installs and configures Chef Server from Omnibus"
long_description       "Installs and configures Chef Server from Omnibus"
version           "0.1.0"
recipe            "chef-server", "Configures the Chef Server from Omnibus"

%w{ ubuntu debian redhat centos oracle scientific fedora amazon }.each do |os|
  supports os
end

depends          'enterprise' # grabbed via Berkshelf + Git
depends          'apt'
depends          'yum', '~> 3.0'
depends          'openssl', '>= 4.4'
