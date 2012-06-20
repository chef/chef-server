#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

nrpe_dir = node['private_chef']['nrpe']['dir']
nrpe_etc_dir = File.join(nrpe_dir, "etc")
nrpe_bin_dir = File.join(nrpe_dir, "bin")
nrpe_log_dir = node['private_chef']['nrpe']['log_directory']

[ 
  nrpe_dir,
  nrpe_etc_dir,
  nrpe_bin_dir,
  nrpe_log_dir
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0755'
    recursive true
  end
end

nrpe_config = File.join(nrpe_etc_dir, "nrpe.cfg")

template nrpe_config do
  source "nrpe.cfg.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['nrpe'].to_hash)
  notifies :restart, 'service[nrpe]' if OmnibusHelper.should_notify?("nrpe")
end

template File.join(nrpe_bin_dir, "nrpe.sh") do
  source "nrpe.sh.erb"
  owner "root"
  group "root"
  mode "4755"
end

runit_service "nrpe" do
  options({
    :log_directory => nrpe_log_dir,
    :svlogd_size => node['private_chef']['nrpe']['svlogd_size'],
    :svlogd_num  => node['private_chef']['nrpe']['svlogd_num']
  }.merge(params))
end

