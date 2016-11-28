#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

# BEGIN AUTHZ CLEANUP
#
# Remove all traces of the legacy opscode-authz service that oc_bifrost
# replaces.
#
execute "/opt/opscode/bin/private-chef-ctl stop opscode-authz" do
  retries 5
  only_if{ ::Dir.exist? '/opt/opscode/sv/opscode-authz' }
end

component_runit_supervisor "opscode-authz" do
  action :delete
end

directory "/opt/opscode/sv/opscode-authz" do
  action :delete
  recursive true
end
# END AUTHZ CLEANUP

# BEGIN oc_bifrost CLEAN
# It is now part of oc_erchef
execute "/opt/opscode/bin/chef-server-ctl stop oc_bifrost" do
  retries 5
  only_if{ ::Dir.exist? '/opt/opscode/sv/oc_bifrost' }
end


component_runit_supervisor "oc_bifrost" do
  action :delete
end

directory node['private_chef']['oc_bifrost']['dir'] do
  action :delete
  recursive true
end

directory '/opt/opscode/sv/oc_bifrost' do
  action :delete
  recursive true
end
directory '/opt/opscode/service/oc_bifrost' do
  action :delete
  recursive true
end

file File.join(node['private_chef']['oc_bifrost']['dir'], "sys.config") do
  action :delete
end
file "/opt/opscode/embedded/service/oc_bifrost/sys.config" do
  action :delete
end

