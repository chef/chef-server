#
# Author:: Nathan Haneysmith (<nathan@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

# sudo /opt/opscode/embedded/sbin/varnishd -F -P /opt/opscode/embedded/var/run/varnishd.pid -a :6984 -T 6082 -f /opt/opscode/embedded/etc/varnish/default.vcl -s file,/opt/opscode/embedded/var/lib/varnish/private-chef/varnish_storage.bin,1G

varnish_dir = node['private_chef']['varnish']['dir']
varnish_etc_dir = File.join(varnish_dir, "etc")
varnish_cache_dir = File.join(varnish_dir, "cache")
varnish_log_dir = node['private_chef']['varnish']['log_directory']

### mkdir /opt/opscode/embedded/var/lib/varnish/private-chef/
### template /opt/opscode/embedded/etc/varnish/default.vcl
#### IP/port

[
  varnish_dir,
  varnish_etc_dir,
  varnish_cache_dir,
  varnish_log_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

varnish_config = File.join(varnish_etc_dir, "default.vcl")

varnish_vars = node['private_chef']['varnish'].to_hash

template File.join(varnish_etc_dir, "default.vcl") do
  source "varnish-default.vcl.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['varnish'].to_hash)
  notifies :restart, 'service[varnish]' if OmnibusHelper.should_notify?("varnish")
end

runit_service "varnish" do
  down node['private_chef']['varnish']['ha']
  options({
    :log_directory => varnish_log_dir,
    :svlogd_size => node['private_chef']['varnish']['svlogd_size'],
    :svlogd_num  => node['private_chef']['varnish']['svlogd_num']
  }.merge(params))
end

if node['private_chef']['varnish']['bootstrap']
        execute "/opt/opscode/bin/private-chef-ctl varnish start" do
                retries 20
        end
end

add_nagios_hostgroup("varnish")

