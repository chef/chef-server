#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Opscode, Inc.
#
# All Rights Reserved
#

case node['platform_family']
when 'debian'
  package_suffix = 'deb'
when 'rhel'
  package_suffix = 'rpm'
else
  # TODO: probably don't actually want to fail out?
  raise "I don't know how to install addons for platform family: #{node['platform_family']}"
end

if (node['private_chef']['add_ons']['path'])
  node['private_chef']['add_ons']['packages'].each do |pkg|
    # find the newest package of each name
    pkg_file = Dir["#{addon_path}/#{pkg}*.#{package_suffix}"].sort_by_time.first
    package pkg do
      source pkg_file
    end
  end
end
