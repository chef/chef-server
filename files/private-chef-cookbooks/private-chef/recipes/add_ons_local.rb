#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Opscode, Inc.
#
# All Rights Reserved
#

if (node['private_chef']['add_ons']['path'])
  node['private_chef']['add_ons']['packages'].each do |pkg|
    # find the newest package of each name
    pkg_file = Dir["#{addon_path}/#{pkg}*.deb"].sort_by_time.first
    package pkg do
      source pkg_file
    end
  end
end
