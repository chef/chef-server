#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

###
# High level options
###
default['private_chef']['notification_email'] = "pc-default@opscode.com"
default['private_chef']['database_type'] = "postgresql"

####
# The Chef User that services run as
####
# The username for the chef services user
default['private_chef']['user']['username'] = "opscode"
# The shell for the chef services user
default['private_chef']['user']['shell'] = "/bin/sh"
# The home directory for the chef services user
default['private_chef']['user']['home'] = "/opt/opscode/embedded"

####
# CouchDB 
####
# Enable/disable the CouchDB service
default['private_chef']['couchdb']['enable'] = true 
default['private_chef']['couchdb']['ha'] = false 
# The directory for CouchDB data
default['private_chef']['couchdb']['dir'] = "/var/opt/opscode/couchdb"
default['private_chef']['couchdb']['data_dir'] = "/var/opt/opscode/couchdb/db"
default['private_chef']['couchdb']['log_directory'] = "/var/log/opscode/couchdb"
# The port to listen on
default['private_chef']['couchdb']['port'] = 5984
# The IP Address to bind on - use 0.0.0.0 for everything 
default['private_chef']['couchdb']['bind_address'] = '127.0.0.1'
# The VIP
default['private_chef']['couchdb']['vip'] = "127.0.0.1"
default['private_chef']['couchdb']['max_document_size'] = '4294967296'
default['private_chef']['couchdb']['max_attachment_chunk_size'] = '4294967296'
default['private_chef']['couchdb']['os_process_timeout'] = '300000'
default['private_chef']['couchdb']['max_dbs_open'] = 10000
default['private_chef']['couchdb']['delayed_commits'] = 'true'
default['private_chef']['couchdb']['batch_save_size'] = 1000 
default['private_chef']['couchdb']['batch_save_interval'] = 1000 
default['private_chef']['couchdb']['log_level'] = 'error' 
default['private_chef']['couchdb']['reduce_limit'] = 'false' 

####
# RabbitMQ
####
default['private_chef']['rabbitmq']['enable'] = true
default['private_chef']['rabbitmq']['ha'] = false 
default['private_chef']['rabbitmq']['dir'] = "/var/opt/opscode/rabbitmq"
default['private_chef']['rabbitmq']['data_dir'] = "/var/opt/opscode/rabbitmq/db"
default['private_chef']['rabbitmq']['log_directory'] = "/var/log/opscode/rabbitmq"
default['private_chef']['rabbitmq']['vhost'] = '/chef'
default['private_chef']['rabbitmq']['user'] = 'chef'
default['private_chef']['rabbitmq']['password'] = 'chefrocks'
default['private_chef']['rabbitmq']['reindexer_vhost'] = '/reindexer'
default['private_chef']['rabbitmq']['jobs_vhost'] = '/jobs'
default['private_chef']['rabbitmq']['jobs_user'] = 'jobs'
default['private_chef']['rabbitmq']['jobs_password'] = 'workcomplete'
default['private_chef']['rabbitmq']['node_ip_address'] = '127.0.0.1'
default['private_chef']['rabbitmq']['node_port'] = '5672'
default['private_chef']['rabbitmq']['nodename'] = 'rabbit@localhost'
default['private_chef']['rabbitmq']['vip'] = '127.0.0.1'
default['private_chef']['rabbitmq']['consumer_id'] = 'hotsauce'

####
# Chef Solr
####
default['private_chef']['opscode-solr']['enable'] = true
default['private_chef']['opscode-solr']['ha'] = false
default['private_chef']['opscode-solr']['dir'] = "/var/opt/opscode/opscode-solr"
default['private_chef']['opscode-solr']['data_dir'] = "/var/opt/opscode/opscode-solr/data"
default['private_chef']['opscode-solr']['log_directory'] = "/var/log/opscode/opscode-solr"

# node[:memory][:total] =~ /^(\d+)kB/
# memory_total_in_kb = $1.to_i
# solr_mem = (memory_total_in_kb - 600000) / 1024
# # cap solr memory at 6G
# if solr_mem > 6144
#   solr_mem = 6144
# end
default['private_chef']['opscode-solr']['heap_size'] = "256M"
default['private_chef']['opscode-solr']['java_opts'] = ""
default['private_chef']['opscode-solr']['url'] = "http://localhost:8983"
default['private_chef']['opscode-solr']['ip_address'] = '127.0.0.1'
default['private_chef']['opscode-solr']['vip'] = '127.0.0.1'
default['private_chef']['opscode-solr']['port'] = 8983
default['private_chef']['opscode-solr']['ram_buffer_size'] = 200 
default['private_chef']['opscode-solr']['merge_factor'] = 100
default['private_chef']['opscode-solr']['max_merge_docs'] = 2147483647
default['private_chef']['opscode-solr']['max_field_length'] = 100000
default['private_chef']['opscode-solr']['max_commit_docs'] = 1000
default['private_chef']['opscode-solr']['commit_interval'] = 60000 # in ms
default['private_chef']['opscode-solr']['poll_seconds'] = 20 # slave -> master poll interval in seconds, max of 60 (see solrconfig.xml.erb)

####
# Chef Expander
####
default['private_chef']['opscode-expander']['enable'] = true
default['private_chef']['opscode-expander']['ha'] = false
default['private_chef']['opscode-expander']['dir'] = "/var/opt/opscode/opscode-expander"
default['private_chef']['opscode-expander']['log_directory'] = "/var/log/opscode/opscode-expander"
default['private_chef']['opscode-expander']['reindexer_log_directory'] = "/var/log/opscode/opscode-expander-reindexer"
default['private_chef']['opscode-expander']['consumer_id'] = "default" 
default['private_chef']['opscode-expander']['nodes'] = 2 

####
# Chef Server API
####
default['private_chef']['opscode-chef']['enable'] = true
default['private_chef']['opscode-chef']['ha'] = false
default['private_chef']['opscode-chef']['dir'] = "/var/opt/opscode/opscode-chef"
default['private_chef']['opscode-chef']['log_directory'] = "/var/log/opscode/opscode-chef"
default['private_chef']['opscode-chef']['sandbox_path'] = "/var/opt/opscode/opscode-chef/sandbox"
default['private_chef']['opscode-chef']['checksum_path'] = "/var/opt/opscode/opscode-chef/checksum"
default['private_chef']['opscode-chef']['proxy_user'] = "pivotal"
default['private_chef']['opscode-chef']['environment'] = 'privatechef'
default['private_chef']['opscode-chef']['url'] = "http://127.0.0.1:9460" 
default['private_chef']['opscode-chef']['upload_vip'] = "127.0.0.1" 
default['private_chef']['opscode-chef']['upload_port'] = 9460 
default['private_chef']['opscode-chef']['upload_proto'] = "http" 
default['private_chef']['opscode-chef']['upload_internal_vip'] = "127.0.0.1" 
default['private_chef']['opscode-chef']['upload_internal_port'] = 9460 
default['private_chef']['opscode-chef']['upload_internal_proto'] = "http" 
default['private_chef']['opscode-chef']['vip'] = "127.0.0.1" 
default['private_chef']['opscode-chef']['port'] = 9460
default['private_chef']['opscode-chef']['listen'] = '127.0.0.1:9460'
default['private_chef']['opscode-chef']['backlog'] = 1024
default['private_chef']['opscode-chef']['tcp_nodelay'] = true 
default['private_chef']['opscode-chef']['worker_timeout'] = 3600
default['private_chef']['opscode-chef']['validation_client_name'] = "chef"
default['private_chef']['opscode-chef']['umask'] = "0022"
default['private_chef']['opscode-chef']['worker_processes'] = node["cpu"]["total"].to_i
default['private_chef']['opscode-chef']['web_ui_client_name'] = "chef-webui"
default['private_chef']['opscode-chef']['web_ui_admin_user_name'] = "admin"
default['private_chef']['opscode-chef']['web_ui_admin_default_password'] = "p@ssw0rd1"

####
# Erlang Chef Server API
####
default['private_chef']['opscode-erchef']['enable'] = true
default['private_chef']['opscode-erchef']['ha'] = false
default['private_chef']['opscode-erchef']['dir'] = "/var/opt/opscode/opscode-erchef"
default['private_chef']['opscode-erchef']['log_directory'] = "/var/log/opscode/opscode-erchef"
default['private_chef']['opscode-erchef']['vip'] = '127.0.0.1'
default['private_chef']['opscode-erchef']['listen'] = '127.0.0.1'
default['private_chef']['opscode-erchef']['port'] = 8000
default['private_chef']['opscode-erchef']['auth_skew'] = '900'
default['private_chef']['opscode-erchef']['bulk_fetch_batch_size'] = '5'
default['private_chef']['opscode-erchef']['max_cache_size'] = '10000'
default['private_chef']['opscode-erchef']['cache_ttl'] = '3600'
default['private_chef']['opscode-erchef']['db_pool_size'] = '20'
default['private_chef']['opscode-erchef']['couchdb_max_conn'] = '100'

####
# Chef Server WebUI
####
default['private_chef']['opscode-webui']['enable'] = true
default['private_chef']['opscode-webui']['ha'] = false
default['private_chef']['opscode-webui']['dir'] = "/var/opt/opscode/opscode-webui"
default['private_chef']['opscode-webui']['log_directory'] = "/var/log/opscode/opscode-webui"
default['private_chef']['opscode-webui']['environment'] = 'privatechef'
default['private_chef']['opscode-webui']['url'] = "http://127.0.0.1:9462" 
default['private_chef']['opscode-webui']['listen'] = '127.0.0.1:9462'
default['private_chef']['opscode-webui']['vip'] = '127.0.0.1'
default['private_chef']['opscode-webui']['port'] = 9462
default['private_chef']['opscode-webui']['backlog'] = 1024
default['private_chef']['opscode-webui']['tcp_nodelay'] = true 
default['private_chef']['opscode-webui']['worker_timeout'] = 3600
default['private_chef']['opscode-webui']['validation_client_name'] = "chef"
default['private_chef']['opscode-webui']['umask'] = "0022"
default['private_chef']['opscode-webui']['worker_processes'] = node["cpu"]["total"].to_i
default['private_chef']['opscode-webui']['session_key'] = "_sandbox_session"
default['private_chef']['opscode-webui']['cookie_domain'] = "all"
default['private_chef']['opscode-webui']['cookie_secret'] = "47b3b8d95dea455baf32155e95d1e64e" 

###
# Load Balancer
###
default['private_chef']['lb']['enable'] = true
default['private_chef']['lb']['vip'] = "127.0.0.1"
default['private_chef']['lb']['api_fqdn'] = node['fqdn'] 
default['private_chef']['lb']['web_ui_fqdn'] = node['fqdn'] 
default['private_chef']['lb']['cache_cookbook_files'] = false
default['private_chef']['lb']['debug'] = false
default['private_chef']['lb']['upstream']['opscode-chef'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['opscode-erchef'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['opscode-account'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['opscode-webui'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['opscode-authz'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['opscode-solr'] = [ "127.0.0.1" ]
default['private_chef']['lb_internal']['enable'] = true
default['private_chef']['lb_internal']['vip'] = "127.0.0.1"
default['private_chef']['lb_internal']['chef_port'] = 9680
default['private_chef']['lb_internal']['account_port'] = 9685
default['private_chef']['lb_internal']['authz_port'] = 9683

####
# Nginx
####
default['private_chef']['nginx']['enable'] = true
default['private_chef']['nginx']['ha'] = false
default['private_chef']['nginx']['dir'] = "/var/opt/opscode/nginx"
default['private_chef']['nginx']['log_directory'] = "/var/log/opscode/nginx"
default['private_chef']['nginx']['ssl_port'] = 443
default['private_chef']['nginx']['enable_non_ssl'] = false
default['private_chef']['nginx']['non_ssl_port'] = 80
default['private_chef']['nginx']['server_name'] = node['fqdn']
default['private_chef']['nginx']['url'] = "https://#{node['fqdn']}"
# These options provide the current best security with TSLv1
#default['private_chef']['nginx']['ssl_protocols'] = "-ALL +TLSv1"
#default['private_chef']['nginx']['ssl_ciphers'] = "RC4:!MD5"
# This might be necessary for auditors that want no MEDIUM security ciphers and don't understand BEAST attacks
#default['private_chef']['nginx']['ssl_protocols'] = "-ALL +SSLv3 +TLSv1"
#default['private_chef']['nginx']['ssl_ciphers'] = "HIGH:!MEDIUM:!LOW:!ADH:!kEDH:!aNULL:!eNULL:!EXP:!SSLv2:!SEED:!CAMELLIA:!PSK"
# The following favors performance and compatibility, addresses BEAST, and should pass a PCI audit
default['private_chef']['nginx']['ssl_protocols'] = "SSLv3 TLSv1"
default['private_chef']['nginx']['ssl_ciphers'] = "RC4-SHA:RC4-MD5:RC4:RSA:HIGH:MEDIUM:!LOW:!kEDH:!aNULL:!ADH:!eNULL:!EXP:!SSLv2:!SEED:!CAMELLIA:!PSK"
default['private_chef']['nginx']['ssl_certificate'] = nil 
default['private_chef']['nginx']['ssl_certificate_key'] = nil 
default['private_chef']['nginx']['ssl_country_name'] = "US"
default['private_chef']['nginx']['ssl_state_name'] = "WA"
default['private_chef']['nginx']['ssl_locality_name'] = "Seattle"
default['private_chef']['nginx']['ssl_company_name'] = "YouCorp"
default['private_chef']['nginx']['ssl_organizational_unit_name'] = "Operations"
default['private_chef']['nginx']['ssl_email_address'] = "you@example.com"
default['private_chef']['nginx']['worker_processes'] = node['cpu']['total'].to_i
default['private_chef']['nginx']['worker_connections'] = 10240 
default['private_chef']['nginx']['sendfile'] = 'on'
default['private_chef']['nginx']['tcp_nopush'] = 'on'
default['private_chef']['nginx']['tcp_nodelay'] = 'on'
default['private_chef']['nginx']['gzip'] = "on"               
default['private_chef']['nginx']['gzip_http_version'] = "1.0"
default['private_chef']['nginx']['gzip_comp_level'] = "2"   
default['private_chef']['nginx']['gzip_proxied'] = "any"   
default['private_chef']['nginx']['gzip_types'] = [ "text/plain", "text/css", "application/x-javascript", "text/xml", "application/xml", "application/xml+rss", "text/javascript", "application/json" ]
default['private_chef']['nginx']['keepalive_timeout'] = 65 
default['private_chef']['nginx']['client_max_body_size'] = '250m' 
default['private_chef']['nginx']['cache_max_size'] = '5000m' 

###
# MySQL
###
default['private_chef']['mysql']['enable'] = false
default['private_chef']['mysql']['sql_user'] = "opscode_chef"
default['private_chef']['mysql']['sql_password'] = "snakepliskin"
default['private_chef']['mysql']['vip'] = "127.0.0.1"
default['private_chef']['mysql']['destructive_migrate'] = false 
default['private_chef']['mysql']['install_libs'] = true 
default['private_chef']['mysql']['mysql2_versions'] = IO.readlines("/opt/opscode/pc-version.txt").detect { |l| l =~ /^mysql2/ }.gsub(/^mysql2:\s+(\d.+)$/, '\1').chomp.split("-")

###
# PostgreSQL
###
default['private_chef']['postgresql']['enable'] = true
default['private_chef']['postgresql']['ha'] = false
default['private_chef']['postgresql']['dir'] = "/var/opt/opscode/postgresql"
default['private_chef']['postgresql']['data_dir'] = "/var/opt/opscode/postgresql/data"
default['private_chef']['postgresql']['log_directory'] = "/var/log/opscode/postgresql"
default['private_chef']['postgresql']['username'] = "opscode-pgsql"
default['private_chef']['postgresql']['shell'] = "/bin/sh"
default['private_chef']['postgresql']['home'] = "/opt/opscode/embedded"
default['private_chef']['postgresql']['sql_user'] = "opscode_chef"
default['private_chef']['postgresql']['sql_password'] = "snakepliskin"
default['private_chef']['postgresql']['sql_ro_user'] = "opscode_chef_ro"
default['private_chef']['postgresql']['sql_ro_password'] = "shmunzeltazzen"
default['private_chef']['postgresql']['vip'] = "127.0.0.1"
default['private_chef']['postgresql']['port'] = 5432
default['private_chef']['postgresql']['listen_address'] = 'localhost'
default['private_chef']['postgresql']['max_connections'] = 200
default['private_chef']['postgresql']['md5_auth_cidr_addresses'] = [ ]
default['private_chef']['postgresql']['trust_auth_cidr_addresses'] = [ '127.0.0.1/32', '::1/128' ]
default['private_chef']['postgresql']['shmmax'] = 17179869184
default['private_chef']['postgresql']['shmall'] = 4194304

###
# Redis 
###
default['private_chef']['redis']['enable'] = true
default['private_chef']['redis']['ha'] = false
default['private_chef']['redis']['dir'] = "/var/opt/opscode/redis"
default['private_chef']['redis']['log_directory'] = "/var/log/opscode/redis"
default['private_chef']['redis']['port'] = "6379"
default['private_chef']['redis']['bind'] = "127.0.0.1"
default['private_chef']['redis']['vip'] = "127.0.0.1"
default['private_chef']['redis']['timeout'] = "300"
default['private_chef']['redis']['loglevel'] = "notice"
default['private_chef']['redis']['databases'] = "16"
default['private_chef']['redis']['appendonly'] = "no"
default['private_chef']['redis']['appendfsync'] = "everysec"
default['private_chef']['redis']['vm']['enabled'] = "no"
default['private_chef']['redis']['vm']['max_memory'] = "0"
default['private_chef']['redis']['vm']['page_size'] = "32"
default['private_chef']['redis']['vm']['pages'] = "134217728"
default['private_chef']['redis']['vm']['max_threads'] = "4"
default['private_chef']['redis']['root'] = '/var/opt/opscode/redis'
default['private_chef']['redis']['maxmemory'] = "1g"
default['private_chef']['redis']['maxmemory_policy'] = "volatile-lru"

###
# Opscode Authorization
###
default['private_chef']['opscode-authz']['enable'] = true
default['private_chef']['opscode-authz']['ha'] = false
default['private_chef']['opscode-authz']['dir'] = "/var/opt/opscode/opscode-authz"
default['private_chef']['opscode-authz']['log_directory'] = "/var/log/opscode/opscode-authz"
default['private_chef']['opscode-authz']['caching'] = "enabled"
default['private_chef']['opscode-authz']['port'] = 9463 
default['private_chef']['opscode-authz']['vip'] = '127.0.0.1'
default['private_chef']['opscode-authz']['superuser_id'] = '5ca1ab1ef005ba111abe11eddecafbad'
default['private_chef']['opscode-authz']['couchdb_max_conn'] = '100'

###
# Opscode Certificate
###
default['private_chef']['opscode-certificate']['enable'] = true
default['private_chef']['opscode-certificate']['ha'] = false
default['private_chef']['opscode-certificate']['dir'] = "/var/opt/opscode/opscode-certificate"
default['private_chef']['opscode-certificate']['log_directory'] = "/var/log/opscode/opscode-certificate"
default['private_chef']['opscode-certificate']['port'] = 5140 
default['private_chef']['opscode-certificate']['vip'] = '127.0.0.1'
default['private_chef']['opscode-certificate']['num_workers'] = '2'
default['private_chef']['opscode-certificate']['num_certificates_per_worker'] = '50'

###
# Opscode Organization Creator
###
default['private_chef']['opscode-org-creator']['enable'] = true
default['private_chef']['opscode-org-creator']['ha'] = false
default['private_chef']['opscode-org-creator']['dir'] = "/var/opt/opscode/opscode-org-creator"
default['private_chef']['opscode-org-creator']['log_directory'] = "/var/log/opscode/opscode-org-creator"
default['private_chef']['opscode-org-creator']['ready_org_depth'] = 10 
default['private_chef']['opscode-org-creator']['max_workers'] = 1 
default['private_chef']['opscode-org-creator']['create_wait_ms'] = 30000
default['private_chef']['opscode-org-creator']['create_splay_ms'] = 25000
default['private_chef']['opscode-org-creator']['port'] = 4369

###
# Dark Launch
###
default['private_chef']['dark_launch']["quick_start"] = false
default['private_chef']['dark_launch']["new_theme"] = true
default['private_chef']['dark_launch']["private-chef"] = true
default['private_chef']['dark_launch']["sql_users"] = true

###
# Opscode Account
###
default['private_chef']['opscode-account']['enable'] = true
default['private_chef']['opscode-account']['ha'] = false
default['private_chef']['opscode-account']['dir'] = "/var/opt/opscode/opscode-account"
default['private_chef']['opscode-account']['log_directory'] = "/var/log/opscode/opscode-account"
default['private_chef']['opscode-account']['proxy_user'] = "pivotal"
default['private_chef']['opscode-account']['session_secret_key'] = 'change-by-default'
default['private_chef']['opscode-account']['environment'] = 'privatechef'
default['private_chef']['opscode-account']['vip'] = '127.0.0.1'
default['private_chef']['opscode-account']['port'] = 9465 
default['private_chef']['opscode-account']['url'] = "http://127.0.0.1:9465" 
default['private_chef']['opscode-account']['listen'] = '127.0.0.1:9465'
default['private_chef']['opscode-account']['backlog'] = 1024
default['private_chef']['opscode-account']['tcp_nodelay'] = true 
default['private_chef']['opscode-account']['worker_timeout'] = 3600
default['private_chef']['opscode-account']['validation_client_name'] = "chef"
default['private_chef']['opscode-account']['umask'] = "0022"
default['private_chef']['opscode-account']['worker_processes'] = node['cpu']['total'].to_i

###
# Opscode Test
###
default['private_chef']['bootstrap']['enable'] = true

###
# Estatsd
###
default['private_chef']['estatsd']['enable'] = true
default['private_chef']['estatsd']['dir'] = "/var/opt/opscode/estatsd"
default['private_chef']['estatsd']['log_directory'] = "/var/log/opscode/estatsd"
default['private_chef']['estatsd']['vip'] = "127.0.0.1"
default['private_chef']['estatsd']['port'] = 9466 

###
# Nagios
###
default['private_chef']['nagios']['enable'] = true
default['private_chef']['nagios']['ha'] = false
default['private_chef']['nagios']['dir'] = "/var/opt/opscode/nagios"
default['private_chef']['nagios']['log_directory'] = "/var/log/opscode/nagios"
default['private_chef']['nagios']['php_fpm_log_directory'] = "/var/log/opscode/php-fpm"
default['private_chef']['nagios']['fcgiwrap_log_directory'] = "/var/log/opscode/fcgiwrap"
default['private_chef']['nagios']['admin_user'] = "nagiosadmin"
default['private_chef']['nagios']['admin_password'] = "privatechef"
default['private_chef']['nagios']['admin_email'] = "nobody@example.com"
default['private_chef']['nagios']['admin_pager'] = "nobody@example.com"
default['private_chef']['nagios']['debug_level'] = 0
default['private_chef']['nagios']['debug_verbosity'] = 1 
default['private_chef']['nagios']['alert_email'] = "nobody@example.com" 
default['private_chef']['nagios']['interval_length'] = 1
default['private_chef']['nagios']['default_host']['check_interval']     = 15
default['private_chef']['nagios']['default_host']['retry_interval']     = 15
default['private_chef']['nagios']['default_host']['max_check_attempts'] = 1
default['private_chef']['nagios']['default_host']['notification_interval'] = 300
default['private_chef']['nagios']['default_service']['check_interval']     = 60
default['private_chef']['nagios']['default_service']['retry_interval']     = 15
default['private_chef']['nagios']['default_service']['max_check_attempts'] = 3
default['private_chef']['nagios']['default_service']['notification_interval'] = 1200
default['private_chef']['nagios']['port'] = 9671
default['private_chef']['nagios']['fcgiwrap_port'] = 9670
default['private_chef']['nagios']['php_fpm_port'] = 9000
default['private_chef']['nagios']['hosts'][node['hostname']] = { 
  "ipaddress" => node['ipaddress'],
  "hostgroups" => [ ] 
}

###
# NRPE
###
default['private_chef']['nrpe']['enable'] = true
default['private_chef']['nrpe']['dir'] = "/var/opt/opscode/nrpe" 
default['private_chef']['nrpe']['log_directory'] = "/var/log/opscode/nrpe"
default['private_chef']['nrpe']['port'] = 9672
default['private_chef']['nrpe']['listen'] = node['ipaddress']
default['private_chef']['nrpe']['allowed_hosts'] = ["127.0.0.1", node['ipaddress']]

##
# DRBD
##
default['private_chef']['drbd']['enable'] = false
default['private_chef']['drbd']['dir'] = "/var/opt/opscode/drbd" 
default['private_chef']['drbd']['data_dir'] = "/var/opt/opscode/drbd/data" 
default['private_chef']['drbd']['sync_rate'] = "40M"
default['private_chef']['drbd']['shared_secret'] = "promisespromises"
default['private_chef']['drbd']['device'] = "/dev/drbd0"
default['private_chef']['drbd']['disk'] = "/dev/opscode/drbd"
default['private_chef']['drbd']['flexible_meta_disk'] = "internal"
default['private_chef']['drbd']['primary']['fqdn'] = node['fqdn']
default['private_chef']['drbd']['primary']['ip'] = node['ipaddress'] 
default['private_chef']['drbd']['primary']['port'] = 7788
default['private_chef']['drbd']['secondary']['fqdn'] = node['fqdn']
default['private_chef']['drbd']['secondary']['ip'] = node['ipaddress'] 
default['private_chef']['drbd']['secondary']['port'] = 7788
if File.exists?("/sbin/drbdadm")
  default['private_chef']['drbd']['version'] = `drbdadm --version | grep DRBDADM_VERSION= | cut -d "=" -f 2`.chomp!
else
  Chef::Log.debug("No DRBD version available!")
  default['private_chef']['drbd']['version'] = nil
end

##
# Keepalived
##
default['private_chef']['keepalived']['enable'] = false
default['private_chef']['keepalived']['dir'] = "/var/opt/opscode/keepalived" 
default['private_chef']['keepalived']['log_directory'] = "/var/log/opscode/keepalived"
default['private_chef']['keepalived']['smtp_server'] = "127.0.0.1"
default['private_chef']['keepalived']['smtp_connect_timeout'] = "30"
default['private_chef']['keepalived']['vrrp_sync_group'] = "PC_GROUP"
default['private_chef']['keepalived']['vrrp_sync_instance'] = "PC_VI"
default['private_chef']['keepalived']['vrrp_instance_state'] = "MASTER"
default['private_chef']['keepalived']['vrrp_instance_interface'] = "eth0"
default['private_chef']['keepalived']['vrrp_instance_virtual_router_id'] = "1"
default['private_chef']['keepalived']['vrrp_instance_priority'] = "100"
default['private_chef']['keepalived']['vrrp_instance_advert_int'] = "1"
default['private_chef']['keepalived']['vrrp_instance_password'] = "sneakybeaky"
default['private_chef']['keepalived']['vrrp_instance_ipaddress'] = node['ipaddress'] 
default['private_chef']['keepalived']['vrrp_instance_ipaddress_dev'] = 'eth0' 
default['private_chef']['keepalived']['vrrp_instance_vrrp_unicast_bind'] = node['ipaddress']
default['private_chef']['keepalived']['vrrp_instance_vrrp_unicast_peer'] = nil

default['private_chef']['keepalived']['service_order'] = [
	{ "key" => "couchdb", "service_name" => "couchdb" },
	{ "key" => "postgresql", "service_name" => "postgres" },
	{ "key" => "rabbitmq", "service_name" => "rabbitmq" },
	{ "key" => "redis", "service_name" => "redis" },
	{ "key" => "opscode-authz", "service_name" => "opscode-authz" },
	{ "key" => "opscode-certificate", "service_name" => "opscode-certificate" },
  { "key" => "opscode-account", "service_name" => "opscode-account" },
  { "key" => "opscode-solr", "service_name" => "opscode-solr" },
  { "key" => "opscode-expander", "service_name" => "opscode-expander" },
  { "key" => "opscode-expander", "service_name" => "opscode-expander-reindexer" },
  { "key" => "opscode-org-creator", "service_name" => "opscode-org-creator" },
  { "key" => "opscode-chef", "service_name" => "opscode-chef" },
  { "key" => "opscode-erchef", "service_name" => "opscode-erchef" },
  { "key" => "opscode-webui", "service_name" => "opscode-webui" },
  { "key" => "nagios", "service_name" => "php-fpm" },
  { "key" => "nagios", "service_name" => "fcgiwrap" },
  { "key" => "nagios", "service_name" => "nagios" },
  { "key" => "nginx", "service_name" => "nginx" }
]

default['private_chef']['keepalived']['service_posthooks'] = {
    "rabbitmq" => "PATH=/opt/opscode/embedded/bin:$PATH /opt/opscode/embedded/bin/rabbitmqctl wait /var/opt/opscode/rabbitmq/db/rabbit@localhost.pid"
}

