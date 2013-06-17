#
# Copyright:: Copyright (c) 2013 Opscode, Inc.
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

bifrost_dir = node['private_chef']['bifrost']['dir']
bifrost_bin_dir = File.join(bifrost_dir, "bin")
bifrost_etc_dir = File.join(bifrost_dir, "etc")
bifrost_log_dir = node['private_chef']['bifrost']['log_directory']
bifrost_sasl_log_dir = File.join(bifrost_log_dir, "sasl")
[
  bifrost_dir,
  bifrost_bin_dir,
  bifrost_etc_dir,
  bifrost_log_dir,
  bifrost_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link "/opt/opscode/embedded/service/bifrost/log" do
  to bifrost_log_dir
end

template "/opt/opscode/embedded/service/bifrost/bin/oc_bifrost" do
  source "oc_bifrost.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['bifrost'].to_hash)
  notifies :restart, 'service[bifrost]' if OmnibusHelper.should_notify?("bifrost")
end

bifrost_config = File.join(bifrost_etc_dir, "sys.config")

template bifrost_config do
  source "oc_bifrost.config.erb"
  mode "644"
  variables(node['private_chef']['bifrost'].to_hash)
  notifies :restart, 'service[bifrost]' if OmnibusHelper.should_notify?("bifrost")
end

link "/opt/opscode/embedded/service/bifrost/etc/sys.config" do
  to bifrost_config
end

runit_service "bifrost" do
  down node['private_chef']['bifrost']['ha']
  options({
    :log_directory => bifrost_log_dir,
    :svlogd_size => node['private_chef']['bifrost']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['bifrost']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
  execute "/opt/opscode/bin/private-chef-ctl start bifrost" do
    retries 20
  end
end

add_nagios_hostgroup("bifrost")
