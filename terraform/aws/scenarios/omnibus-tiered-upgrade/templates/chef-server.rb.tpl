topology = "tier"

server "backend.internal",
  :ipaddress => "${back_end_ip}/${cidr}",
  :role => "backend",
  :bootstrap => true

backend_vip "backend.internal",
  :ipaddress => "${back_end_ip}/${cidr}"

server "frontend.internal",
  :ipaddress => "${front_end_ip}/${cidr}",
  :role => "frontend"

api_fqdn = "frontend.internal"

profiles['root_url'] = 'http://frontend.internal:9998'

opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

insecure_addon_compat = false

data_collector['token'] = 'foobar'

nginx['enable_ipv6'] = ${enable_ipv6}
