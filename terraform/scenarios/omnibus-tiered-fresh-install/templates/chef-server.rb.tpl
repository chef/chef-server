topology = "tier"

server "backend.internal",
  :ipaddress => "${back_end_ipv6}/64",
  :role => "backend",
  :bootstrap => true

backend_vip "backend.internal",
  :ipaddress => "${back_end_ipv6}/64"

server "frontend.internal",
  :ipaddress => "${front_end_ipv6}/64",
  :role => "frontend"

api_fqdn = "frontend.internal"

opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

insecure_addon_compat = false

data_collector['token'] = 'foobar'

nginx['enable_ipv6'] = true
