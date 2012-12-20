#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved

opscode_org_creator_dir = node['private_chef']['opscode-org-creator']['dir']
opscode_org_creator_etc_dir = File.join(opscode_org_creator_dir, "etc")
opscode_org_creator_log_dir = node['private_chef']['opscode-org-creator']['log_directory']
opscode_org_creator_log_sasl_dir = File.join(node['private_chef']['opscode-org-creator']['log_directory'], "sasl")
[ 
  opscode_org_creator_dir,
  opscode_org_creator_etc_dir,
  opscode_org_creator_log_dir,
  opscode_org_creator_log_sasl_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

org_creator_config = File.join(opscode_org_creator_etc_dir, "app.config") 

template org_creator_config do
  source "opscode-org-creator.config.erb"
  mode "644"
  variables(node['private_chef']['opscode-org-creator'].to_hash)
  notifies :restart, 'service[opscode-org-creator]' if OmnibusHelper.should_notify?("opscode-org-creator")
end

link "/opt/opscode/embedded/service/opscode-org-creator/rel/org_app/etc/app.config" do
  to org_creator_config 
end

link "/opt/opscode/embedded/service/opscode-org-creator/rel/org_app/log" do
  to opscode_org_creator_log_dir
end

template "/opt/opscode/embedded/service/opscode-org-creator/rel/org_app/bin/org_app" do
  source 'org_app.erb'
  mode 0755
  owner 'root'
  group 'root'
  notifies :restart, 'service[opscode-org-creator]' if OmnibusHelper.should_notify?("opscode-org-creator")
end

runit_service "opscode-org-creator" do
  down node['private_chef']['opscode-org-creator']['ha']
  options({
    :log_directory => opscode_org_creator_log_dir,
    :svlogd_size => node['private_chef']['opscode-org-creator']['svlogd_size'],
    :svlogd_num  => node['private_chef']['opscode-org-creator']['svlogd_num']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
		retries 20 
	execute "/opt/opscode/bin/private-chef-ctl start opscode-org-creator" do
	end
end

add_nagios_hostgroup("opscode-org-creator")

