#
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved
#

node.set['runit']['sv_bin'] = "/opt/opscode/embedded/bin/sv"
node.set['runit']['chpst_bin'] = "/opt/opscode/embedded/bin/chpst"
node.set['runit']['service_dir'] = "/opt/opscode/service"
node.set['runit']['sv_dir'] = "/opt/opscode/sv"
node.set['runit']['lsb_init_dir'] = "/opt/opscode/init"

case node["platform_family"]
when "debian"
  include_recipe "private-chef::runit_upstart"
when "rhel"
  if node['platform_version'] =~ /^6/
    include_recipe "private-chef::runit_upstart"
  else
    include_recipe "private-chef::runit_sysvinit"
  end
else
  include_recipe "private-chef::runit_sysvinit"
end
