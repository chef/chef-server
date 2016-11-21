
default['provisioning']['chef-server-config'] = {}

default['ldap']['basedn']   = 'dc=chef-server,dc=dev'
default['ldap']['ssl_key']  = '/etc/ldap/ssl/chef-server_dev.key'
default['ldap']['ssl_cert'] = '/etc/ldap/ssl/chef-server_dev.crt'
