#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
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

addon_path = node['private_chef']['addons']['path']
node['private_chef']['addons']['packages'].each do |pkg|
  # find the newest package of each name
  pkg_file = Dir["#{addon_path}/#{pkg}*.#{package_suffix}"].sort_by{ |f| File.mtime(f) }.last
  package pkg do
    case node['platform_family']
    when 'debian'
      provider Chef::Provider::Package::Dpkg
    when 'rhel'
      provider Chef::Provider::Package::Rpm
    end
    source pkg_file
    notifies :create, "ruby_block[addon_install_notification_#{pkg}]", :immediate
  end
end
