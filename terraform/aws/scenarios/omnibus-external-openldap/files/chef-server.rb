opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

data_collector['token'] = 'foobar' unless data_collector.nil?

profiles['root_url'] = 'http://localhost:9998' unless profiles.nil?

ldap['base_dn'] = 'ou=chefs,dc=chef-server,dc=dev'
ldap['bind_dn'] = 'cn=admin,dc=chef-server,dc=dev'
ldap['bind_password'] = 'H0\/\/!|\/|3tY0ur|\/|0th3r'
ldap['host'] = 'ldap.chef-server.dev'
ldap['login_attribute'] = 'uid'

# Use TLS for encryption against an OpenLDAP instance to avoid connection resets
ldap['ssl_enabled'] = false
ldap['tls_enabled'] = true
