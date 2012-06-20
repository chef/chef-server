#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

keepalived_dir = node['private_chef']['keepalived']['dir']
keepalived_etc_dir = File.join(keepalived_dir, "etc")
keepalived_bin_dir = File.join(keepalived_dir, "bin")
keepalived_log_dir = node['private_chef']['keepalived']['log_directory']

[ keepalived_dir, keepalived_etc_dir, keepalived_bin_dir, keepalived_log_dir ].each do |dir|
	directory dir do
		recursive true
		mode "0755"
	end
end

template File.join(keepalived_etc_dir, "keepalived.conf") do
	source "keepalived.conf.erb"
	mode "0644"
  variables(node['private_chef']['keepalived'].to_hash)
	notifies :restart, 'service[keepalived]' if OmnibusHelper.should_notify?("kepalived")
end

template File.join(keepalived_bin_dir, "cluster.sh") do
	source "cluster.sh.erb"
	mode "0755"
  variables(node['private_chef']['keepalived'].to_hash)
end

runit_service "keepalived" do
  options({
    :log_directory => keepalived_log_dir,
    :svlogd_size => node['private_chef']['keepalived']['svlogd_size'],
    :svlogd_num  => node['private_chef']['keepalived']['svlogd_num']
  }.merge(params))
end
