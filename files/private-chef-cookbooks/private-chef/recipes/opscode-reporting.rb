#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved

opscode_reporting_dir = node['private_chef']['opscode-reporting']['dir']
opscode_reporting_etc_dir = File.join(opscode_reporting_dir, "etc")
opscode_reporting_log_dir = node['private_chef']['opscode-reporting']['log_directory']
opscode_reporting_sasl_log_dir = File.join(opscode_reporting_log_dir, "sasl")
[
  opscode_reporting_dir,
  opscode_reporting_etc_dir,
  opscode_reporting_log_dir,
  opscode_reporting_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-reporting/log" do
  to opscode_reporting_log_dir
end

template "/opt/opscode/embedded/service/opscode-reporting/bin/reporting" do
  source "reporting.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['opscode-reporting'].to_hash)
  notifies :restart, 'service[opscode-reporting]' if OmnibusHelper.should_notify?("opscode-reporting")
end

reporting_config = File.join(opscode_reporting_etc_dir, "app.config")

template reporting_config do
  source "reporting.config.erb"
  mode "644"
  variables(node['private_chef']['opscode-reporting'].to_hash)
  notifies :restart, 'service[opscode-reporting]' if OmnibusHelper.should_notify?("opscode-reporting")
end

link "/opt/opscode/embedded/service/opscode-reporting/etc/app.config" do
  to reporting_config
end


runit_service "opscode-reporting" do
  down node['private_chef']['opscode-reporting']['ha']
  options({
    :log_directory => opscode_reporting_log_dir
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl start opscode-reporting" do
		retries 20
	end
end

add_nagios_hostgroup("opscode-reporting")

