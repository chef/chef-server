topology 'standalone'

opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

data_collector['token'] = 'foobar'

profiles['root_url'] = 'http://localhost:9998'

postgresql['external']=true
postgresql['sslmode']='require'
postgresql['vip']='${postgres_fqdn}'
postgresql['db_superuser']='bofh'
postgresql['db_superuser_password']='i1uvd3v0ps!'
opscode_erchef['db_pool_size']=10
oc_id['db_pool_size']=10
oc_bifrost['db_pool_size']=10

postgresql['db_connection_superuser']='bofh@${postgres_fqdn}'

postgresql['sql_connection_user']='bofh@${postgres_fqdn}'

bookshelf['sql_connection_user']='bookshelf@${postgres_fqdn}'

oc_bifrost['sql_connection_user']='bifrost@${postgres_fqdn}'

oc_id['sql_connection_user']='oc_id@${postgres_fqdn}'

opscode_erchef['sql_connection_user']='opscode_chef@${postgres_fqdn}'

