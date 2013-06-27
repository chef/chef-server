#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

couchdb_dir = node['private_chef']['couchdb']['dir']
couchdb_etc_dir =  File.join(node['private_chef']['couchdb']['dir'], "etc")
couchdb_data_dir = node['private_chef']['couchdb']['data_dir']
couchdb_data_dir_symlink = File.join(node['private_chef']['couchdb']['dir'], "db")
couchdb_log_dir = node['private_chef']['couchdb']['log_directory']

# Create the CouchDB directories
[ couchdb_dir, couchdb_etc_dir, couchdb_data_dir, couchdb_log_dir ].each do |dir_name|
  directory dir_name do
    mode "0700"
    recursive true
    owner node['private_chef']['user']['username']
  end
end

link couchdb_data_dir_symlink do
  to couchdb_data_dir
  not_if { couchdb_data_dir_symlink == couchdb_data_dir }
end

# Drop off the CouchDB configuration file
template File.join(couchdb_etc_dir, "local.ini") do
  source "local.ini.erb"
  owner node['private_chef']['user']['username']
  mode "0600"
  variables(node['private_chef']['couchdb'].to_hash)
  notifies :restart, "service[couchdb]" if OmnibusHelper.should_notify?("couchdb")
end

component_runit_service "couchdb"

# Cron may not be installed in a minimal install:
case node["platform"]
when "ubuntu"
when "centos", "redhat", "scientific"
  if node["platform_version"] =~ /^5/
    package "vixie-cron"
  else
    package "cronie"
  end
end

compact_script_command = File.join(couchdb_etc_dir, "compact_couch.rb")

# Drop off the CouchDB compaction script
template compact_script_command do
  source "compact_couchdb_and_views.rb.erb"
  mode "0755"
end

# Add it to cron
cron_email = node['private_chef']['notification_email']
cron_cmd = "if `test -e #{couchdb_data_dir}/_users.couch` ; then #{compact_script_command} 2>&1 > #{couchdb_log_dir}/compact-`date \"+\\%Y\\%m\\%d\\%H\\%M\\%S\"`.log ; fi"
cron_cmd_major_offenders = "if `test -e #{couchdb_data_dir}/_users.couch` ; then #{compact_script_command} --max-dbs=50 2>&1 > #{couchdb_log_dir}/compact-`date \"+\\%Y\\%m\\%d\\%H\\%M\\%S\"`.log ; fi"

template "/etc/cron.d/couchdb_compact" do
  source "compact-cron-entry.erb"
  mode "0600"
  variables(
            :cron_email => cron_email,
            :cron_name => "compact couchdb",
            :cron_shell => "/bin/bash",
            :cron_home => couchdb_dir,
            :cron_schedule => "17 1,9,17 * * *",
            :cron_user => node['private_chef']['user']['username'],
            :cron_path => "/usr/bin:/usr/sbin:/bin:/opt/opscode/embedded/bin",
            :cron_command => cron_cmd
            )
end

template "/etc/cron.d/couchdb_compact_major_offenders" do
  source "compact-cron-entry.erb"
  mode "0600"
  variables(
            :cron_email => cron_email,
            :cron_name => "compact couchdb major offenders",
            :cron_shell => "/bin/bash",
            :cron_home => couchdb_dir,
            :cron_schedule => "17 3,5,7,11,13,15,19,21,23 * * *",
            :cron_user => node['private_chef']['user']['username'],
            :cron_path => "/usr/bin:/usr/sbin:/bin:/opt/opscode/embedded/bin",
            :cron_command => cron_cmd_major_offenders
            )
end

template "/etc/cron.d/couchdb_bounce" do
  source "couchdb-bounce-cron.erb"
  mode "0600"
end

# log rotation
template "/etc/opscode/logrotate.d/couchdb" do
  source "logrotate.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['couchdb'].to_hash)
end
