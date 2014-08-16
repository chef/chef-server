name "chef-ha"
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
depends 'chef-ha-drdb'

# Find any installed chef-ha cookbooks and require them
chef_ha_cookbooks = Dir.glob('/opt/chef-ha/embedded/cookbooks/chef-ha-*').map{|cookbook| cookbook.split('/')[-1] }
chef_ha_cookbooks.each do |cookbook|
  depends cookbook
end
