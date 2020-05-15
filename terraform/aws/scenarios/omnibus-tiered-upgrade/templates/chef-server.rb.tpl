topology "tier"

server "backend.internal",
  :ipaddress => "${back_end_ip}/${cidr}",
  :role => "backend",
  :bootstrap => true

backend_vip "backend.internal",
  :ipaddress => "${back_end_ip}/${cidr}"

server "frontend.internal",
  :ipaddress => "${front_end_ip}/${cidr}",
  :role => "frontend"

api_fqdn "frontend.internal"

# The public IPV6 address is not bound to the network interface of the machine.
# rhel-7 and rhel-8 are able to resolve this but the older rhel-6 is not.
# Using the ipv6 localhost instead of the public IP to fix this.
require 'ipaddr'
if IPAddr.new("${front_end_ip}/${cidr}").ipv6?
  profiles['root_url'] = 'http://[::1]:9998'
else
  profiles['root_url'] = 'http://frontend.internal:9998'
end

opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

insecure_addon_compat = false

data_collector['token'] = 'foobar' unless data_collector.nil?

nginx['enable_ipv6'] = ${enable_ipv6}
