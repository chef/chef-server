#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Chef, Inc.
#
# All Rights Reserved
#

if (node['private_chef']['addons']['path'])
  include_recipe "private-chef::add_ons_local"
else
  include_recipe "private-chef::add_ons_remote"
end

