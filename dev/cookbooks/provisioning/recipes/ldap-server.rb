
#
# Install Packages
#
package 'ldap-utils' do
  action :install
end

package 'slapd' do
  response_file 'slapd.seed'
  action :install
end

#
# Setup SSL Certificates
#
directory '/etc/ldap/ssl' do
  mode '0755'
  owner 'root'
  group 'root'
  recursive true
end

execute 'create snakeoil certificate' do
  command <<-EOH
/opt/chef/embedded/bin/openssl req -subj "/CN=chef-server.dev" \
  -new \
  -newkey rsa:2048 \
  -days 365 \
  -nodes \
  -x509 \
  -keyout #{node['ldap']['ssl_key']} \
  -out #{node['ldap']['ssl_cert']}

chmod 600 #{node['ldap']['ssl_key']}
chown openldap:openldap #{node['ldap']['ssl_key']}
chown openldap:openldap #{node['ldap']['ssl_cert']}
  EOH
  not_if { File.exist?(node['ldap']['ssl_key']) }
end

#
# Start LDAP service
#
service 'slapd' do
  action [:enable, :start]
end

#
# Configure LDAP
#
directory '/etc/ldap/config' do
  recursive true
  owner 'openldap'
  group 'openldap'
end

%w(
  logging
  tls
).each do |config|

  template "/etc/ldap/config/#{config}.ldif" do
    source "ldap-config/#{config}.ldif.erb"
    owner 'openldap'
    group 'openldap'
    mode '0644'
    notifies :run, "execute[configure-#{config}]", :immediately
  end

  execute "configure-#{config}" do
    command "ldapmodify -Y EXTERNAL -H ldapi:/// -f /etc/ldap/config/#{config}.ldif -Q"
    action :nothing
  end
end

#
# Setup Users
#
directory '/etc/ldap/data' do
  owner 'openldap'
  group 'openldap'
  recursive true
end

# Add chefs OU
cookbook_file '/etc/ldap/data/ou-chefs.ldif' do
  source 'ldap-data/ou-chefs.ldif'
  owner 'openldap'
  group 'openldap'
  notifies :run, 'execute[configure-ou]', :immediately
end

execute 'configure-ou' do
  command "ldapadd -x -H ldapi:/// -D cn=admin,#{node['ldap']['basedn']} -w #{node['ldap']['password']} -f /etc/ldap/data/ou-chefs.ldif"
  action :nothing
end

# Create Users
%w(
  user1
  child
  douglas
).each do |user|
  cookbook_file "/etc/ldap/data/user-#{user}.ldif" do
    source "ldap-data/user-#{user}.ldif"
    owner 'openldap'
    group 'openldap'
    notifies :run, "execute[configure-#{user}]"
  end

  execute "configure-#{user}" do
    command "ldapadd -x -H ldapi:/// -D cn=admin,#{node['ldap']['basedn']} -w #{node['ldap']['password']} -f /etc/ldap/data/user-#{user}.ldif"
    user 'openldap'
    action :nothing
  end
end
