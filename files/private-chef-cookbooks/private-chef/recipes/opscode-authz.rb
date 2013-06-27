#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

opscode_authz_dir = node['private_chef']['opscode-authz']['dir']
opscode_authz_etc_dir = File.join(opscode_authz_dir, "etc")
opscode_authz_bin_dir = File.join(opscode_authz_dir, "bin")
opscode_authz_etc_ibrowse_dir = File.join(opscode_authz_etc_dir, "ibrowse")
opscode_authz_log_dir = node['private_chef']['opscode-authz']['log_directory']
[
  opscode_authz_dir,
  opscode_authz_bin_dir,
  opscode_authz_etc_dir,
  opscode_authz_etc_ibrowse_dir,
  opscode_authz_log_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end


link "/opt/opscode/embedded/service/opscode-authz/rel/authz/log" do
  to opscode_authz_log_dir
end

authz_config = File.join(opscode_authz_etc_dir, "app.config")

template authz_config do
  source "authz.config.erb"
  mode "644"
  variables(node['private_chef']['opscode-authz'].to_hash)
  notifies :restart, 'runit_service[opscode-authz]' if OmnibusHelper.should_notify?("opscode-authz")
end

link "/opt/opscode/embedded/service/opscode-authz/rel/authz/etc/app.config" do
  to authz_config
end

authz = File.join(opscode_authz_bin_dir, "authz")

template authz do
  source "authz.erb"
  mode "755"
  variables(node['private_chef']['opscode-authz'].to_hash)
  notifies :restart, 'runit_service[opscode-authz]' if OmnibusHelper.should_notify?("opscode-authz")
end

link "/opt/opscode/embedded/service/opscode-authz/rel/authz/bin/authz" do
  to authz
end

authz_ibrowse_config = File.join(opscode_authz_etc_dir, "ibrowse.config")

template authz_ibrowse_config do
  source "ibrowse.config.erb"
  mode "0644"
  variables(node['private_chef']['opscode-authz'].to_hash)
  notifies :restart, 'runit_service[opscode-authz]' if OmnibusHelper.should_notify?("opscode-authz")
end

link "/opt/opscode/embedded/service/opscode-authz/rel/authz/etc/ibrowse/ibrowse.config" do
  to authz_ibrowse_config
end

component_runit_service "opscode-authz"

if node['private_chef']['bootstrap']['enable']

  execute "/opt/opscode/embedded/bin/rake design:load" do
    cwd "/opt/opscode/embedded/service/opscode-authz"
    not_if "curl http://#{node['private_chef']['couchdb']['vip']}:#{node['private_chef']['couchdb']['port']}/_all_dbs | grep authorization_design_documents"
  end

end
