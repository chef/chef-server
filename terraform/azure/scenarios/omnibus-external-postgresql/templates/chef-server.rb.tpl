topology 'standalone'

opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

data_collector['token'] = 'foobar'

profiles['root_url'] = 'http://localhost:9998'

postgresql['external']=true
postgresql['sslmode']='require'
postgresql['vip']='${postgresql_fqdn}'
postgresql['db_superuser']='bofh'
postgresql['db_superuser_password']='i1uvd3v0ps!'
postgresql['max_connections']=350
opscode_erchef['db_pool_size']=20
oc_id['db_pool_size']=20
oc_bifrost['db_pool_size']=20

postgresql['db_connection_superuser']='bofh@${postgresql_fqdn}'

postgresql['sql_connection_user']='bofh@${postgresql_fqdn}'

bookshelf['sql_connection_user']='bookshelf@${postgresql_fqdn}'

oc_bifrost['sql_connection_user']='bifrost@${postgresql_fqdn}'

oc_id['sql_connection_user']='oc_id@${postgresql_fqdn}'

opscode_erchef['sql_connection_user']='opscode_chef@${postgresql_fqdn}'

