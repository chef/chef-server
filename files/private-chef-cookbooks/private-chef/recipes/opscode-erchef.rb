#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved

opscode_erchef_dir = node['private_chef']['opscode-erchef']['dir']
opscode_erchef_etc_dir = File.join(opscode_erchef_dir, "etc")
opscode_erchef_log_dir = node['private_chef']['opscode-erchef']['log_directory']
opscode_erchef_sasl_log_dir = File.join(opscode_erchef_log_dir, "sasl")
[ 
  opscode_erchef_dir,
  opscode_erchef_etc_dir,
  opscode_erchef_log_dir,
  opscode_erchef_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-erchef/log" do
  to opscode_erchef_log_dir 
end

template "/opt/opscode/embedded/service/opscode-erchef/bin/oc_erchef" do
  source "oc_erchef.erb"
  owner "root"
  group "root"
  mode "0755" 
  variables(node['private_chef']['opscode-erchef'].to_hash)
  notifies :restart, 'service[opscode-erchef]' if OmnibusHelper.should_notify?("opscode-erchef")
end

erchef_config = File.join(opscode_erchef_etc_dir, "app.config") 

template erchef_config do
  source "oc_erchef.config.erb"
  mode "644"
  variables(node['private_chef']['opscode-erchef'].to_hash)
  notifies :restart, 'service[opscode-erchef]' if OmnibusHelper.should_notify?("opscode-erchef")
end

link "/opt/opscode/embedded/service/opscode-erchef/etc/app.config" do
  to erchef_config 
end

runit_service "opscode-erchef" do
  down node['private_chef']['opscode-erchef']['ha']
  options({
    :log_directory => opscode_erchef_log_dir,
    :svlogd_size => node['private_chef']['opscode-erchef']['svlogd_size'],
    :svlogd_num  => node['private_chef']['opscode-erchef']['svlogd_num']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable'] 
	execute "/opt/opscode/bin/private-chef-ctl opscode-erchef start" do
		retries 20 
	end
end

add_nagios_hostgroup("opscode-erchef")

