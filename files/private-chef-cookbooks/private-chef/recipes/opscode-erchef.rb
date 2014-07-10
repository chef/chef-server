#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved

owner = node['private_chef']['user']['username']
group = owner

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
    owner owner
    mode "0700"
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-erchef/log" do
  to opscode_erchef_log_dir
end

template "/opt/opscode/embedded/service/opscode-erchef/bin/oc_erchef" do
  source "oc_erchef.erb"
  owner owner
  group group
  mode "0755"
  variables(node['private_chef']['opscode-erchef'].to_hash)
  notifies :restart, 'runit_service[opscode-erchef]' unless backend_secondary?
end

erchef_config = File.join(opscode_erchef_etc_dir, "app.config")

template erchef_config do
  source "oc_erchef.config.erb"
  owner owner
  group group
  mode "644"
  variables(node['private_chef']['opscode-erchef'].to_hash.merge(:ldap_enabled => ldap_authentication_enabled?, :helper => OmnibusHelper.new(node)))
  notifies :run, 'execute[remove_erchef_siz_files]', :immediately
  notifies :restart, 'runit_service[opscode-erchef]' unless backend_secondary?
end

# Erchef still ultimately uses disk_log [1] for request logging, and if
# you change the log file sizing in the configuration **without also
# issuing a call to disk_log:change_size/2, Erchef won't start.
#
# Since we currently don't perform live upgrades, we can fake this by
# removing the *.siz files, which is where disk_log looks to determine
# what size the log files should be in the first place.  If they're
# not there, then we just use whatever size is listed in the
# configuration.
#
# [1]: http://erlang.org/doc/man/disk_log.html
execute "remove_erchef_siz_files" do
  command "rm -f *.siz"
  cwd node['private_chef']['opscode-erchef']['log_directory']
  action :nothing
end

link "/opt/opscode/embedded/service/opscode-erchef/etc/app.config" do
  to erchef_config
end

component_runit_service "opscode-erchef"
