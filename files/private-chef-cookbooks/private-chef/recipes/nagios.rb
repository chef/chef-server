#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

user "opscode-nagios" do
  system true
  shell "/bin/sh"
  home node['private_chef']['nagios']['dir']
end

group "opscode-nagios" do
  members [ "opscode-nagios" ]
end

user "opscode-nagios-cmd" do
  system true
  shell "/bin/sh"
  home node['private_chef']['nagios']['dir']
end

group "opscode-nagios-cmd" do
  members [ "opscode-nagios-cmd" ]
end

nagios_dir = node['private_chef']['nagios']['dir']
nagios_etc_dir = File.join(nagios_dir, "etc")
[
  nagios_dir,
  nagios_etc_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    group 'opscode-nagios'
    mode '0775'
    recursive true
  end
end

directory File.join(nagios_dir, "rw") do
  owner "opscode-nagios"
  group node['private_chef']['user']['username']
  mode '2775'
end

directory File.join(nagios_dir, "tmp") do
  owner "opscode-nagios"
  mode '0755'
end

directory File.join(nagios_dir, "fastcgi") do
  owner node['private_chef']['user']['username']
  mode '0755'
end

directory File.join(nagios_dir, "php-fpm") do
  owner node['private_chef']['user']['username']
  mode '0755'
end

directory File.join(nagios_dir, "checkresult") do
  owner "opscode-nagios"
  mode '0755'
end

log_directory = node['private_chef']['nagios']['log_directory']
directory log_directory do
  owner "root"
  group "opscode-nagios"
  mode "0775"
  recursive true
  action :create
end

fcgiwrap_log_directory = File.join(log_directory, "fcgiwrap")
directory fcgiwrap_log_directory do
  owner "root"
  group "opscode-nagios"
  mode "0775"
  recursive true
  action :create
end

php_fpm_log_directory = File.join(log_directory, "php-fpm")
directory php_fpm_log_directory do
  owner node['private_chef']['user']['username']
  group "opscode-nagios"
  mode "0775"
  recursive true
  action :create
end

file File.join(log_directory, "nagios.log") do
  owner "opscode-nagios"
  group "opscode-nagios"
  mode "0644"
end

directory File.join(log_directory, "archives") do
  owner "root"
  group "opscode-nagios"
  mode "0775"
  recursive true
  action :create
end

%w[nagios.cfg resource.cfg commands.cfg contacts.cfg timeperiods.cfg templates.cfg services.cfg hostgroups.cfg hosts.cfg php-fpm.conf php.ini cgi.cfg].each do |cfg_file|
  template File.join(nagios_etc_dir, cfg_file) do
    owner "root"
    group "root"
    mode "0644"
    variables(node['private_chef']['nagios'].to_hash)
    source "nagios/#{cfg_file}.erb"
    notifies :restart, 'service[nagios]' if OmnibusHelper.should_notify?("nagios")
  end
end

link "/opt/opscode/embedded/nagios/etc/cgi.cfg" do
  to File.join(nagios_etc_dir, "cgi.cfg")
end

ruby_block "build htpasswd file" do
  block do
    require 'webrick/httpauth/htpasswd'
    htpasswd = WEBrick::HTTPAuth::Htpasswd.new(File.join(nagios_etc_dir, "htpasswd"))
    unless htpasswd.get_passwd('all', node['private_chef']['nagios']['admin_user'], false)
      htpasswd.set_passwd('all', node['private_chef']['nagios']['admin_user'],  node['private_chef']['nagios']['admin_password'])
      htpasswd.flush
    end
  end
end

file File.join(nagios_etc_dir, "htpasswd") do
  owner "root"
  group "root"
  mode "0644"
end

runit_service "nagios" do
  down node['private_chef']['nagios']['ha']
  options({
    :log_directory => log_directory,
    :svlogd_size => node['private_chef']['nagios']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['nagios']['log_rotation']['num_to_keep']
  }.merge(params))
end

runit_service "fcgiwrap" do
  down node['private_chef']['nagios']['ha']
  options({
    :log_directory => fcgiwrap_log_directory,
    :svlogd_size => node['private_chef']['nagios']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['nagios']['log_rotation']['num_to_keep']
  }.merge(params))
end

runit_service "php-fpm" do
  down node['private_chef']['nagios']['ha']
  options({
    :log_directory => php_fpm_log_directory,
    :svlogd_size => node['private_chef']['nagios']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['nagios']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl start nagios" do
		retries 20
	end
	execute "/opt/opscode/bin/private-chef-ctl start php-fpm" do
		retries 20
	end
	execute "/opt/opscode/bin/private-chef-ctl start fcgiwrap" do
		retries 20
	end
end

# log rotation
template "/etc/opscode/logrotate.d/nagios" do
  source "logrotate.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['nagios'].to_hash.merge(
    'owner' => 'opscode-nagios',
    'group' => 'opscode-nagios'
  ))
end
