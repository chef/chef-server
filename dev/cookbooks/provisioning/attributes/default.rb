
default['provisioning']["#{Chef::Dist::Server::SHORT}-config"] = {}

default['ldap']['basedn']   = "dc=#{Chef::Dist::Server::SHORT},dc=dev"
default['ldap']['ssl_key']  = "/etc/ldap/ssl/#{Chef::Dist::Server::SHORT}_dev.key"
default['ldap']['ssl_cert'] = "/etc/ldap/ssl/#{Chef::Dist::Server::SHORT}_dev.crt"
default['ldap']['password']   = 'H0\/\/!|\/|3tY0ur|\/|0th3r'