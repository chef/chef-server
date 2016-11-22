#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: Copyright (c) 2011-2016 Chef Software, Inc.
#
# All Rights Reserved

oc_erchef = node['private_chef']['opscode-erchef']
oc_bifrost = node['private_chef']['oc_bifrost']
opscode_erchef_dir =  oc_erchef['dir']
opscode_erchef_log_dir = oc_erchef['log_directory']
opscode_erchef_sasl_log_dir = File.join(opscode_erchef_log_dir, "sasl")
[
  opscode_erchef_dir,
  opscode_erchef_log_dir,
  opscode_erchef_sasl_log_dir
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

link "/opt/opscode/embedded/service/opscode-erchef/log" do
  to opscode_erchef_log_dir
end

ldap_authentication_enabled = OmnibusHelper.new(node).ldap_authentication_enabled?
 # These values are validated and managed in libraries/private_chef.rb#gen_ldap
enable_ssl = ldap_authentication_enabled ? node['private_chef']['ldap']['enable_ssl'] : nil
ldap_encryption_type = ldap_authentication_enabled ? node['private_chef']['ldap']['encryption_type'] : nil

erchef_config = File.join(opscode_erchef_dir, "sys.config")

rabbitmq = OmnibusHelper.new(node).rabbitmq_configuration

actions_vip = rabbitmq['vip']
actions_port = rabbitmq['node_port']
actions_user = rabbitmq['actions_user']
actions_password = rabbitmq['actions_password']
actions_vhost = rabbitmq['actions_vhost']
actions_exchange = rabbitmq['actions_exchange']

template erchef_config do
  source "oc_erchef.config.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "644"
  variables({
    oc_erchef: oc_erchef,
    oc_bifrost: oc_bifrost,
    data_collector: node['private_chef']['data_collector'],

    # Snowflakes to get rid of with top-level vars:
    ldap_enabled: ldap_authentication_enabled,
    ldap_encryption_type: ldap_encryption_type,
    log_rotation: oc_erchef['log_rotation'],
    log_directory: opscode_erchef_log_dir,
    # We need to keep these because of the logic above in setting
    # them based on other attributes.
    enable_ssl: enable_ssl,
    # We can just pass in rabbitmq from above -
    # we'll want to do rabbit_index and rabbit_actions because they can be different:
    actions_vip: actions_vip,
    actions_port: actions_port,
    actions_user: actions_user,
    actions_password: actions_password,
    actions_vhost: actions_vhost,
    actions_exchange: actions_exchange,
    helper: OmnibusHelper.new(node)})
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
  cwd oc_erchef['log_directory']
  action :nothing
end

link "/opt/opscode/embedded/service/opscode-erchef/sys.config" do
  to erchef_config
end

component_runit_service "opscode-erchef"
