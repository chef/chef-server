#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

nginx_dir = node['private_chef']['nginx']['dir']
nginx_etc_dir = File.join(nginx_dir, "etc")
nginx_cache_dir = File.join(nginx_dir, "cache")
nginx_cache_tmp_dir = File.join(nginx_dir, "cache-tmp")
nginx_html_dir = File.join(nginx_dir, "html")
nginx_ca_dir = File.join(nginx_dir, "ca")
nginx_log_dir = node['private_chef']['nginx']['log_directory']
nginx_d_dir = File.join(nginx_etc_dir, "nginx.d")
nginx_addon_dir = File.join(nginx_etc_dir, "addon.d")

[
  nginx_dir,
  nginx_etc_dir,
  nginx_cache_dir,
  nginx_cache_tmp_dir,
  nginx_html_dir,
  nginx_ca_dir,
  nginx_log_dir,
  nginx_d_dir,
  nginx_addon_dir,
].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

ssl_keyfile = File.join(nginx_ca_dir, "#{node['private_chef']['nginx']['server_name']}.key")
ssl_crtfile = File.join(nginx_ca_dir, "#{node['private_chef']['nginx']['server_name']}.crt")
ssl_signing_conf = File.join(nginx_ca_dir, "#{node['private_chef']['nginx']['server_name']}-ssl.conf")

unless File.exists?(ssl_keyfile) && File.exists?(ssl_crtfile) && File.exists?(ssl_signing_conf)
  file ssl_keyfile do
    owner "root"
    group "root"
    mode "0644"
    content `/opt/opscode/embedded/bin/openssl genrsa 2048`
    not_if { File.exists?(ssl_keyfile) }
  end

  file ssl_signing_conf do
    owner "root"
    group "root"
    mode "0644"
    not_if { File.exists?(ssl_signing_conf) }
    content <<-EOH
  [ req ]
  distinguished_name = req_distinguished_name
  prompt = no

  [ req_distinguished_name ]
  C                      = #{node['private_chef']['nginx']['ssl_country_name']}
  ST                     = #{node['private_chef']['nginx']['ssl_state_name']}
  L                      = #{node['private_chef']['nginx']['ssl_locality_name']}
  O                      = #{node['private_chef']['nginx']['ssl_company_name']}
  OU                     = #{node['private_chef']['nginx']['ssl_organizational_unit_name']}
  CN                     = #{node['private_chef']['nginx']['server_name']}
  emailAddress           = #{node['private_chef']['nginx']['ssl_email_address']}
  EOH
  end

  ruby_block "create crtfile" do
    block do
      r = Chef::Resource::File.new(ssl_crtfile, run_context)
      r.owner "root"
      r.group "root"
      r.mode "0644"
      r.content `/opt/opscode/embedded/bin/openssl req -config '#{ssl_signing_conf}' -new -x509 -nodes -sha1 -days 3650 -key #{ssl_keyfile}`
      r.not_if { File.exists?(ssl_crtfile) }
      r.run_action(:create)
    end
  end
end

node.default['private_chef']['nginx']['ssl_certificate'] ||= ssl_crtfile
node.default['private_chef']['nginx']['ssl_certificate_key'] ||= ssl_keyfile

remote_directory nginx_html_dir do
  source "html"
  files_backup false
  files_owner "root"
  files_group "root"
  files_mode "0644"
  owner node['private_chef']['user']['username']
  mode "0700"
end

nginx_config = File.join(nginx_etc_dir, "nginx.conf")
chef_lb_configs = {
  :chef_https_config => File.join(nginx_etc_dir, "chef_https_lb.conf"),
  :chef_http_config => File.join(nginx_etc_dir, "chef_http_lb.conf")
}

nginx_vars = node['private_chef']['nginx'].to_hash
nginx_vars = nginx_vars.merge({ :helper => NginxErb.new(node),
                                :allowed_webui_subnets => PrivateChef.allowed_webui_subnets})

# Chef API lb config for HTTPS and HTTP
["https", "http"].each do |server_proto|
  config_key = "chef_#{server_proto}_config".to_sym
  lb_config = chef_lb_configs[config_key]

  template lb_config do
    source "nginx_chef_api_lb.conf.erb"
    owner "root"
    group "root"
    mode "0644"
    variables(nginx_vars.merge({:server_proto => server_proto}))
    notifies :restart, 'service[nginx]' if OmnibusHelper.should_notify?("nginx")
  end

end

template nginx_config do
  source "nginx.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(nginx_vars.merge(chef_lb_configs))
  notifies :restart, 'service[nginx]' if OmnibusHelper.should_notify?("nginx")
end

template File.join(nginx_addon_dir, "README.md") do
  source "nginx-addons.README.erb"
  owner "root"
  group "root"
  mode "0644"
end


runit_service "nginx" do
  down node['private_chef']['nginx']['ha']
  options({
    :log_directory => nginx_log_dir,
    :svlogd_size => node['private_chef']['nginx']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['nginx']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['nginx']['bootstrap']
        execute "/opt/opscode/bin/private-chef-ctl start nginx" do
                retries 20
        end
end

# log rotation
template "/etc/opscode/logrotate.d/nginx" do
  source "logrotate.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['nginx'].to_hash.merge(
    'postrotate' => "/opt/opscode/embedded/sbin/nginx -s reopen",
    'owner' => 'root',
    'group' => 'root'
  ))
end
