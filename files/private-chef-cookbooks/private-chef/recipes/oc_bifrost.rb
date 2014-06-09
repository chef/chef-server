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
  retries 20
end

runit_service "opscode-authz" do
  action :disable
end

directory "/opt/opscode/sv/opscode-authz" do
  action :delete
  recursive true
end
# END AUTHZ CLEANUP

owner = node['private_chef']['user']['username']
group = owner

oc_bifrost_dir = node['private_chef']['oc_bifrost']['dir']
oc_bifrost_bin_dir = File.join(oc_bifrost_dir, "bin")
oc_bifrost_etc_dir = File.join(oc_bifrost_dir, "etc")
oc_bifrost_log_dir = node['private_chef']['oc_bifrost']['log_directory']
oc_bifrost_sasl_log_dir = File.join(oc_bifrost_log_dir, "sasl")
[
  oc_bifrost_dir,
  oc_bifrost_bin_dir,
  oc_bifrost_etc_dir,
  oc_bifrost_log_dir,
  oc_bifrost_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner owner
    mode "0700"
    recursive true
  end
end

link "/opt/opscode/embedded/service/oc_bifrost/log" do
  to oc_bifrost_log_dir
end

template "/opt/opscode/embedded/service/oc_bifrost/bin/oc_bifrost" do
  source "oc_bifrost.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['oc_bifrost'].to_hash)
  notifies :restart, 'runit_service[oc_bifrost]' unless backend_secondary?
end

oc_bifrost_config = File.join(oc_bifrost_etc_dir, "sys.config")

template oc_bifrost_config do
  source "oc_bifrost.config.erb"
  owner owner
  group group
  mode "644"
  variables(node['private_chef']['oc_bifrost'].to_hash)
  notifies :restart, 'runit_service[oc_bifrost]' unless backend_secondary?
end

link "/opt/opscode/embedded/service/oc_bifrost/etc/sys.config" do
  to oc_bifrost_config
end

component_runit_service "oc_bifrost"
