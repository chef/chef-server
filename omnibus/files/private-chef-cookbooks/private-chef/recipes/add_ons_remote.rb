#
# Author:: Douglas Triggs (<doug@chef.io>)
# Copyright:: Copyright (c) 2014-2014 Chef Software, Inc.
#
# All Rights Reserved
#

# Only "enabled false" on yum repos, because
# the default contents of "/etc/apt/apt.conf.d/50unattended-upgrades"
# on an Ubuntu system would prevent chef-stable packages from being auto upgraded.

# Add the repository that makes the addon installation possible:
include_recipe "private-chef::add_ons_repository"

node['private_chef']['addons']['packages'].each do |pkg|
  package pkg do
    notifies :create, "ruby_block[addon_install_notification_#{pkg}]", :immediate
    case node['platform_family']
    when 'rhel'
      options "--enablerepo=chef-stable"
    end
  end
end
