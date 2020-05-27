topology "tier"

server "${back_end_node_fqdn}",
  :ipaddress => "${back_end_ip}/${cidr}",
  :role => "backend",
  :bootstrap => true

backend_vip "${back_end_node_fqdn}",
  :ipaddress => "${back_end_ip}/${cidr}"

server "${front_end_node_fqdn}",
  :ipaddress => "${front_end_ip}/${cidr}",
  :role => "frontend"

api_fqdn "${front_end_node_fqdn}"

# The public IPV6 address is not bound to the network interface of the machine.
# rhel-7 and rhel-8 are able to resolve this but the older rhel-6 is not.
# Using the ipv6 localhost instead of the public IP to fix this.
require 'ipaddr'
if IPAddr.new("${front_end_ip}/${cidr}").ipv6?
  profiles['root_url'] = 'http://[::1]:9998'
else
  profiles['root_url'] = 'http://localhost:9998'
end

opscode_erchef['keygen_start_size'] = 30

opscode_erchef['keygen_cache_size'] = 60

nginx['ssl_dhparam'] = '/etc/opscode/dhparam.pem'

insecure_addon_compat = false

data_collector['token'] = 'foobar' unless data_collector.nil?

if ${enable_ipv6}
  ip_version "ipv6"
  opscode_erchef['solr_ibrowse_options'] = '[{connect_timeout, 10000}, {prefer_ipv6, true}]'
  opscode_erchef['s3_ssl_options'] = '[{connect_timeout, 10000}]'
  opscode_erchef['s3_ibrowse_options'] = '[{prefer_ipv6, true}]'
end
nginx['enable_ipv6'] = ${enable_ipv6}
