#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

if node['private_chef']['drbd']['version'].nil?
  raise "to use drbd you must install drbd first, please follow the install instructions"
end

drbd_dir = node['private_chef']['drbd']['dir']
drbd_etc_dir =  File.join(node['private_chef']['drbd']['dir'], "etc")
drbd_data_dir = node['private_chef']['drbd']['data_dir']

drbd_role = "primary"
PrivateChef['servers'].each do |k, v|
  next unless v['role'] == "backend"
  node.set['private_chef']['drbd'][drbd_role]["fqdn"] = k
  node.set['private_chef']['drbd'][drbd_role]["ip"] = \
    v['cluster_ipaddress'] || v['ipaddress']
  break if drbd_role == "secondary"
  drbd_role = "secondary"
end

[ drbd_dir, drbd_etc_dir, drbd_data_dir ].each do |dir|
  directory dir do
    recursive true
    mode "0755"
  end
end

shared_secret = PrivateChef.credentials.get('drbd', 'shared_secret')

template File.join(drbd_etc_dir, "drbd.conf") do
  source "drbd.conf.erb"
  owner "root"
  group "root"
  mode "0655"
  variables(node['private_chef']['drbd'].to_hash.merge('shared_secret' => shared_secret))
end

template File.join(drbd_etc_dir, "pc0.res") do
  source "pc0.res.erb"
  owner "root"
  group "root"
  mode "0655"
  variables(node['private_chef']['drbd'].to_hash)
end

execute "mv /etc/drbd.conf /etc/drbd.conf.orig" do
  only_if { File.exists?("/etc/drbd.conf") && !File.symlink?("/etc/drbd.conf") }
end

link "/etc/drbd.conf" do
  to File.join(drbd_etc_dir, "drbd.conf")
end

ruby_block "check_for_drbd_mount" do
  block do
    while true
      Chef::Log.warn("To install DRBD on #{node['platform']} #{node['platform_version']}:")
      case node["platform_family"]
      when "rhel", "fedora"
        if %w{amazon fedora}.include?(node["platform"]) ||
          node["platform_version"] =~ /^6/
          puts <<-EOH

rpm --import http://elrepo.org/RPM-GPG-KEY-elrepo.org
yum install -y http://elrepo.org/elrepo-release-6-4.el6.elrepo.noarch.rpm
yum install -y drbd84-utils kmod-drbd84
service drbd start

          EOH
        end
      when "debian"
        puts <<-EOH

apt-get install drbd8-utils
service drbd start

        EOH
      end
      Chef::Log.warn("Please defer to your Private Chef manual for instructions on initializing the device.")
      Chef::Log.warn("Cannot find #{File.join(drbd_dir, 'drbd_ready')} - please bootstrap DRBD and run 'touch #{File.join(drbd_dir, 'drbd_ready')}'.")
      Chef::Log.warn("Press CTRL-C to abort.")
      break if File.exists?(File.join(drbd_dir, "drbd_ready"))
      sleep 60
    end
  end
  not_if { File.exists?(File.join(drbd_dir, "drbd_ready")) }
end

directory "#{node['private_chef']['keepalived']['dir']}/bin" do
  recursive true
end

template "#{node['private_chef']['keepalived']['dir']}/bin/ha_backend_storage" do
  source "ha_backend_storage_drbd.erb"
  owner "root"
  group "root"
  mode "0755"
  variables(node['private_chef']['drbd'].to_hash)
end
