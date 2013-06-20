#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

opscode_certificate_dir = node['private_chef']['opscode-certificate']['dir']
opscode_certificate_etc_dir = File.join(opscode_certificate_dir, "etc")
opscode_certificate_log_dir = node['private_chef']['opscode-certificate']['log_directory']
[
  opscode_certificate_dir,
  opscode_certificate_etc_dir,
  opscode_certificate_log_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-certificate/priv/log" do
  to opscode_certificate_log_dir
end

certificate_config = File.join(opscode_certificate_etc_dir, "certificate.config")

template certificate_config do
  source "certgen_web.config.erb"
  mode "644"
  variables(node['private_chef']['opscode-certificate'].to_hash)
  notifies :restart, 'service[opscode-certificate]' if OmnibusHelper.should_notify?("opscode-certificate")
end

link "/opt/opscode/embedded/service/opscode-certificate/priv/certgen_web.config" do
  to certificate_config
end

runit_service "opscode-certificate" do
  down node['private_chef']['opscode-certificate']['ha']
  options({
    :log_directory => opscode_certificate_log_dir,
    :svlogd_size => node['private_chef']['opscode-certificate']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['opscode-certificate']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl start opscode-certificate" do
		retries 20
	end
end
