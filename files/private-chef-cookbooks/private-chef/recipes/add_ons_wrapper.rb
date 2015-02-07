#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

require "chef/handler"

class AddonInstallHandler < Chef::Handler
  def initialize
    @packages = []
  end

  def add(pkg)
    @packages.push(pkg)
  end

  def report
    @packages.each do |pkg|
      puts "-- Installed Add-On Package: #{pkg}"
    end
  end
end

addon_handler = AddonInstallHandler.new

# We notify whether or not the run was successful:
Chef::Config[:report_handlers].reject! { |i| i.kind_of?(AddonInstallHandler) }
Chef::Config[:report_handlers] << addon_handler
Chef::Config[:exception_handlers].reject! { |i| i.kind_of?(AddonInstallHandler) }
Chef::Config[:exception_handlers] << addon_handler

# No way to pass params via notifications, so we just make a resource to notify for
# every package we intend to install
node['private_chef']['addons']['packages'].each do |pkg|
  ruby_block "addon_install_notification_#{pkg}" do
    block do
      addon_handler.add(pkg)
    end
    action :nothing
  end
end

# chef-solo and chef-client -z return different things :(
if (node['private_chef']['addons']['path'] == nil) || (node['private_chef']['addons']['path'] == {})
  include_recipe "private-chef::add_ons_remote"
else
  include_recipe "private-chef::add_ons_local"
end
