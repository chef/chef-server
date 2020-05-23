opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

data_collector['token'] = 'foobar'

profiles['root_url'] = 'http://localhost:9998'

opscode_solr4['external'] = true
opscode_solr4['external_url'] = 'http://elasticsearch.internal:9200'
opscode_erchef['search_provider'] = 'elasticsearch'
opscode_erchef['search_queue_mode'] = 'batch'

if ${enable_ipv6}
  ip_version "ipv6"
  opscode_erchef['solr_ibrowse_options'] = '[{connect_timeout, 10000}, {prefer_ipv6, true}]'
end
nginx['enable_ipv6'] = ${enable_ipv6}
