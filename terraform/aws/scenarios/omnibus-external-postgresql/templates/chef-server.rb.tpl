opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

data_collector['token'] = 'foobar' unless data_collector.nil?

profiles['root_url'] = 'http://localhost:9998' unless profiles.nil?

postgresql['external'] = true

postgresql['vip'] = "${postgresql_ip}"
postgresql['port'] = 5432
postgresql['db_superuser'] = "bofh"
postgresql['db_superuser_password'] = "i1uvd3v0ps"
postgresql['external'] = true
postgresql['sslmode'] = "disable"
postgresql['max_connections']=350
opscode_erchef['db_pool_size']=20
oc_id['db_pool_size']=20
oc_bifrost['db_pool_size']=20

if ${enable_ipv6}
  ip_version "ipv6"
  opscode_erchef['solr_ibrowse_options'] = '[{connect_timeout, 10000}, {prefer_ipv6, true}]'
end
nginx['enable_ipv6'] = ${enable_ipv6}
