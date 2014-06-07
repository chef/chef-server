#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

owner = node['private_chef']['user']['username']
group = owner

opscode_certificate_dir = node['private_chef']['opscode-certificate']['dir']
opscode_certificate_etc_dir = File.join(opscode_certificate_dir, "etc")
opscode_certificate_log_dir = node['private_chef']['opscode-certificate']['log_directory']
[
  opscode_certificate_dir,
  opscode_certificate_etc_dir,
  opscode_certificate_log_dir,
].each do |dir_name|
  directory dir_name do
    owner owner
    mode "0700"
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-certificate/priv/log" do
  to opscode_certificate_log_dir
end

certificate_config = File.join(opscode_certificate_etc_dir, "certificate.config")

template certificate_config do
  source "certgen_web.config.erb"
  owner owner
  group group
  mode "644"
  variables(node['private_chef']['opscode-certificate'].to_hash)
  notifies :restart, 'runit_service[opscode-certificate]'
end

link "/opt/opscode/embedded/service/opscode-certificate/priv/certgen_web.config" do
  to certificate_config
end

component_runit_service "opscode-certificate"
