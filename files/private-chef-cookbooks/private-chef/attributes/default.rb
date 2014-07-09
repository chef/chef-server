
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

###
# High level options
###
default['private_chef']['api_version'] = "11.1.0"
default['private_chef']['flavor'] = "ec"

default['private_chef']['notification_email'] = "pc-default@opscode.com"
default['private_chef']['from_email'] = '"Opscode" <donotreply@opscode.com>'
default['private_chef']['role'] = "standalone"

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
default['private_chef']['couchdb']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['couchdb']['log_rotation']['num_to_keep'] = 10

# The port to listen on
default['private_chef']['couchdb']['port'] = 5984
# The IP Address to bind on - use 0.0.0.0 for everything
default['private_chef']['couchdb']['bind_address'] = '127.0.0.1'
# The VIP
default['private_chef']['couchdb']['vip'] = "127.0.0.1"
default['private_chef']['couchdb']['max_document_size'] = '4294967296'
default['private_chef']['couchdb']['max_attachment_chunk_size'] = '4294967296'
default['private_chef']['couchdb']['os_process_timeout'] = '300000'
default['private_chef']['couchdb']['max_dbs_open'] = 104857600
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
default['private_chef']['rabbitmq']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['rabbitmq']['log_rotation']['num_to_keep'] = 10
default['private_chef']['rabbitmq']['vhost'] = '/chef'
default['private_chef']['rabbitmq']['user'] = 'chef'
default['private_chef']['rabbitmq']['password'] = 'chefrocks'
default['private_chef']['rabbitmq']['reindexer_vhost'] = '/reindexer'
default['private_chef']['rabbitmq']['jobs_vhost'] = '/jobs'
default['private_chef']['rabbitmq']['jobs_user'] = 'jobs'
default['private_chef']['rabbitmq']['jobs_password'] = 'workcomplete'
default['private_chef']['rabbitmq']['actions_user'] = 'actions'
default['private_chef']['rabbitmq']['actions_password'] = 'changeme'
default['private_chef']['rabbitmq']['actions_vhost'] = '/analytics'
default['private_chef']['rabbitmq']['actions_exchange'] = 'actions'
default['private_chef']['rabbitmq']['node_ip_address'] = '127.0.0.1'
default['private_chef']['rabbitmq']['node_port'] = '5672'
default['private_chef']['rabbitmq']['nodename'] = 'rabbit@localhost'
default['private_chef']['rabbitmq']['vip'] = '127.0.0.1'
default['private_chef']['rabbitmq']['consumer_id'] = 'hotsauce'

####
# Jetty dummy for logs
####
# Should always be enable = false, we control Jetty+Solr through opscode-solr4
default['private_chef']['jetty']['enable'] = false
default['private_chef']['jetty']['ha'] = false
default['private_chef']['jetty']['log_directory'] = "/var/opt/opscode/opscode-solr4/jetty/logs"

####
# Chef Solr 4
####
default['private_chef']['opscode-solr4']['enable'] = true
default['private_chef']['opscode-solr4']['ha'] = false
default['private_chef']['opscode-solr4']['dir'] = "/var/opt/opscode/opscode-solr4"
default['private_chef']['opscode-solr4']['data_dir'] = "/var/opt/opscode/opscode-solr4/data"
default['private_chef']['opscode-solr4']['log_directory'] = "/var/log/opscode/opscode-solr4"
default['private_chef']['opscode-solr4']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['opscode-solr4']['log_rotation']['num_to_keep'] = 10
# defaults for heap size and new generation size are computed in the chef-solr
# recipe based on node memory
default['private_chef']['opscode-solr4']['heap_size'] = nil
default['private_chef']['opscode-solr4']['new_size'] = nil
default['private_chef']['opscode-solr4']['java_opts'] = ""
default['private_chef']['opscode-solr4']['url'] = "http://localhost:8983"
default['private_chef']['opscode-solr4']['ip_address'] = '127.0.0.1'
default['private_chef']['opscode-solr4']['vip'] = '127.0.0.1'
default['private_chef']['opscode-solr4']['port'] = 8983
default['private_chef']['opscode-solr4']['ram_buffer_size'] = 100
default['private_chef']['opscode-solr4']['merge_factor'] = 15
default['private_chef']['opscode-solr4']['max_merge_docs'] = 2147483647
default['private_chef']['opscode-solr4']['max_field_length'] = 100000
default['private_chef']['opscode-solr4']['max_commit_docs'] = 1000
default['private_chef']['opscode-solr4']['auto_soft_commit'] = 1000
default['private_chef']['opscode-solr4']['commit_interval'] = 60000 # in ms
default['private_chef']['opscode-solr4']['poll_seconds'] = 20 # slave -> master poll interval in seconds, max of 60 (see solrconfig.xml.erb)

####
# Chef Expander
####
default['private_chef']['opscode-expander']['enable'] = true
default['private_chef']['opscode-expander']['ha'] = false
default['private_chef']['opscode-expander']['dir'] = "/var/opt/opscode/opscode-expander"
default['private_chef']['opscode-expander']['log_directory'] = "/var/log/opscode/opscode-expander"
default['private_chef']['opscode-expander']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['opscode-expander']['log_rotation']['num_to_keep'] = 10
default['private_chef']['opscode-expander']['reindexer_log_directory'] = "/var/log/opscode/opscode-expander-reindexer"
default['private_chef']['opscode-expander']['consumer_id'] = "default"
default['private_chef']['opscode-expander']['nodes'] = 2

####
# Erlang Chef Server API
####
default['private_chef']['opscode-erchef']['enable'] = true
default['private_chef']['opscode-erchef']['ha'] = false
default['private_chef']['opscode-erchef']['dir'] = "/var/opt/opscode/opscode-erchef"
default['private_chef']['opscode-erchef']['log_directory'] = "/var/log/opscode/opscode-erchef"
default['private_chef']['opscode-erchef']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['opscode-erchef']['log_rotation']['num_to_keep'] = 10
default['private_chef']['opscode-erchef']['vip'] = '127.0.0.1'
default['private_chef']['opscode-erchef']['listen'] = '127.0.0.1'
default['private_chef']['opscode-erchef']['port'] = 8000
default['private_chef']['opscode-erchef']['auth_skew'] = '900'
default['private_chef']['opscode-erchef']['bulk_fetch_batch_size'] = '5'
default['private_chef']['opscode-erchef']['max_cache_size'] = '10000'
default['private_chef']['opscode-erchef']['cache_ttl'] = '3600'
default['private_chef']['opscode-erchef']['db_pool_size'] = '20'
default['private_chef']['opscode-erchef']['udp_socket_pool_size'] = '20'
default['private_chef']['opscode-erchef']['couchdb_max_conn'] = '100'
default['private_chef']['opscode-erchef']['ibrowse_max_sessions'] = 256
default['private_chef']['opscode-erchef']['ibrowse_max_pipeline_size'] = 1
# Default: generate signed URLs based upon Host: header. Override with a url, "http:// ..."
default['private_chef']['opscode-erchef']['base_resource_url'] = :host_header
default['private_chef']['opscode-erchef']['s3_bucket'] = 'bookshelf'
default['private_chef']['opscode-erchef']['s3_url_ttl'] = 28800
default['private_chef']['opscode-erchef']['s3_parallel_ops_timeout'] = 5000
default['private_chef']['opscode-erchef']['s3_parallel_ops_fanout'] = 20
default['private_chef']['opscode-erchef']['authz_timeout'] = 1000
default['private_chef']['opscode-erchef']['authz_fanout'] = 20
default['private_chef']['opscode-erchef']['root_metric_key'] = "chefAPI"
default['private_chef']['opscode-erchef']['depsolver_worker_count'] = 5
default['private_chef']['opscode-erchef']['depsolver_timeout'] = 5000
default['private_chef']['opscode-erchef']['max_request_size'] = 1000000
default['private_chef']['opscode-erchef']['cleanup_batch_size'] = 0

###
# Legacy path (required for cookbok migration)
###
default['private_chef']['opscode-chef']['checksum_path'] = "/var/opt/opscode/opscode-chef/checksum"

####
# Chef Server WebUI (legacy required for manage install to work)
####
default['private_chef']['opscode-webui']['enable'] = false

####
# Chef Pedant
####
default['private_chef']['oc-chef-pedant']['dir'] = "/var/opt/opscode/oc-chef-pedant"
default['private_chef']['oc-chef-pedant']['log_directory'] = "/var/log/opscode/oc-chef-pedant"
default['private_chef']['oc-chef-pedant']['log_http_requests'] = true
default['private_chef']['oc-chef-pedant']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['oc-chef-pedant']['log_rotation']['num_to_keep'] = 10
default['private_chef']['oc-chef-pedant']['debug_org_creation'] = false

###
# redis_lb
###
default['private_chef']['redis_lb']['enable'] = true
default['private_chef']['redis_lb']['ha'] = false
default['private_chef']['redis_lb']['dir'] = "/var/opt/opscode/redis_lb"
default['private_chef']['redis_lb']['data_dir'] = "/var/opt/opscode/redis_lb/data"
default['private_chef']['redis_lb']['log_directory'] = "/var/log/opscode/redis_lb"
default['private_chef']['redis_lb']['log_rotation']['file_maxbytes'] = 1000000
default['private_chef']['redis_lb']['log_rotation']['num_to_keep'] = 10
default['private_chef']['redis_lb']['port'] = "16379"
default['private_chef']['redis_lb']['bind'] = "127.0.0.1"
default['private_chef']['redis_lb']['vip'] = "127.0.0.1"
default['private_chef']['redis_lb']['keepalive'] = "60"
default['private_chef']['redis_lb']['timeout'] = "300"
default['private_chef']['redis_lb']['loglevel'] = "notice"
default['private_chef']['redis_lb']['databases'] = "16"
default['private_chef']['redis_lb']['appendonly'] = "no"
default['private_chef']['redis_lb']['appendfsync'] = "always"
default['private_chef']['redis_lb']['activerehashing'] = "no"
default['private_chef']['redis_lb']['aof_rewrite_percent'] = "50"
default['private_chef']['redis_lb']['aof_rewrite_min_size'] = "16mb"
default['private_chef']['redis_lb']['maxmemory'] = "8m"
default['private_chef']['redis_lb']['maxmemory_policy'] = "noeviction"

default['private_chef']['redis_lb']['save_frequency'] = {
  "900" => "1",
  "300" => "10",
  "60" => "1000"
}

###
# Load Balancer
###
default['private_chef']['lb']['enable'] = true
default['private_chef']['lb']['vip'] = "127.0.0.1"
default['private_chef']['lb']['api_fqdn'] = node['fqdn']
default['private_chef']['lb']['web_ui_fqdn'] = node['fqdn']
default['private_chef']['lb']['cache_cookbook_files'] = false
default['private_chef']['lb']['debug'] = false
default['private_chef']['lb']['upstream']['opscode-erchef'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['opscode-account'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['oc_bifrost'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['opscode-solr4'] = [ "127.0.0.1" ]
default['private_chef']['lb']['upstream']['bookshelf'] = [ "127.0.0.1" ]
default['private_chef']['lb_internal']['enable'] = true
default['private_chef']['lb_internal']['vip'] = "127.0.0.1"
default['private_chef']['lb_internal']['chef_port'] = 9680
default['private_chef']['lb_internal']['account_port'] = 9685
default['private_chef']['lb_internal']['oc_bifrost_port'] = 9683
default['private_chef']['lb']['redis_connection_timeout'] = 1000
default['private_chef']['lb']['redis_keepalive_timeout'] = 2000
default['private_chef']['lb']['redis_connection_pool_size'] = 250
default['private_chef']['lb']['maint_refresh_interval'] = 600
default['private_chef']['lb']['ban_refresh_interval'] = 600
default['private_chef']['lb']['chef_min_version'] = 10
default['private_chef']['lb']['chef_max_version'] = 11

###
# Load balancer route configuration
###
default['private_chef']['lb']['xdl_defaults']['503_mode'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_containers'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_groups'] = false

####
# Nginx
####
default['private_chef']['nginx']['enable'] = true
default['private_chef']['nginx']['ha'] = false
default['private_chef']['nginx']['dir'] = "/var/opt/opscode/nginx"
default['private_chef']['nginx']['log_directory'] = "/var/log/opscode/nginx"
default['private_chef']['nginx']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['nginx']['log_rotation']['num_to_keep'] = 10
default['private_chef']['nginx']['ssl_port'] = 443
default['private_chef']['nginx']['enable_non_ssl'] = false
default['private_chef']['nginx']['non_ssl_port'] = 80
default['private_chef']['nginx']['x_forwarded_proto'] = 'https'
default['private_chef']['nginx']['server_name'] = node['fqdn']
default['private_chef']['nginx']['url'] = "https://#{node['fqdn']}"

# HIGHEST SECURITY AT ALL COSTS: TLSv1 only to prevent BEAST, can also turn off RC4/MEDIUM/MD5 to really favor security over speed/comptability
#default['private_chef']['nginx']['ssl_protocols'] = "-ALL +TLSv1"
#default['private_chef']['nginx']['ssl_ciphers'] = "RC4-SHA:RC4-MD5:RC4:RSA:HIGH:MEDIUM:!LOW:!kEDH:!aNULL:!ADH:!eNULL:!EXP:!SSLv2:!SEED:!CAMELLIA:!PSK"
# HIGHEST SECURITY THAT IS COMPTATIBLE AND FAST: SSLv3 for compatibility, but RC4 only to definitively block BEAST
#default['private_chef']['nginx']['ssl_protocols'] = "-ALL +SSLv3 +TLSv1"
#default['private_chef']['nginx']['ssl_ciphers'] = "RC4-SHA:RC4-MD5:RC4:RSA:!LOW:!kEDH:!aNULL:!ADH:!eNULL:!EXP:!SSLv2:!SEED:!CAMELLIA:!PSK"
# HIGHEST SECURITY ON PAPER: Favors only HIGH security ciphers, still compatible with non-TLSv1 browsers, slow, vulnerable to BEAST on all ciphers over SSLv3
#default['private_chef']['nginx']['ssl_protocols'] = "-ALL +SSLv3 +TLSv1"
#default['private_chef']['nginx']['ssl_ciphers'] = "HIGH:!MEDIUM:!LOW:!ADH:!kEDH:!aNULL:!eNULL:!EXP:!SSLv2:!SEED:!CAMELLIA:!PSK"
# FAST/COMPTATIBLE/AUDITABLE: Favors performance and compatibility, default is not vulnerable to BEAST attacks, uses RC4/MEDIUM, allows MD5
default['private_chef']['nginx']['ssl_protocols'] = "SSLv3 TLSv1"
default['private_chef']['nginx']['ssl_ciphers'] = "RC4-SHA:RC4-MD5:RC4:RSA:HIGH:MEDIUM:!LOW:!kEDH:!aNULL:!ADH:!eNULL:!EXP:!SSLv2:!SEED:!CAMELLIA:!PSK"
# For any of the above:  drop the "RC4-SHA:RC4-MD5:RC4:RSA" prefix and you should wind up favoring AES256 with ECDHE forward security if you want that and don't
# care about speed.
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
default['private_chef']['nginx']['enable_ipv6'] = false

###
# PostgreSQL
###
# For now, we're hardcoding the version directory suffix here:
default['private_chef']['postgresql']['version'] = "9.2"
# In the future, we're probably going to want to do something more elegant so we
# don't accidentally overwrite this directory if we upgrade PG to 9.3: keeping these
# directories straight is important because in the distant future (the year 2000)
# we'll be using these directories to determine what versions we have installed and
# whether we need to run pg_upgrade.
default['private_chef']['postgresql']['enable'] = true
default['private_chef']['postgresql']['ha'] = false
default['private_chef']['postgresql']['dir'] = "/var/opt/opscode/postgresql/#{node['private_chef']['postgresql']['version']}"
default['private_chef']['postgresql']['data_dir'] = "/var/opt/opscode/postgresql/#{node['private_chef']['postgresql']['version']}/data"
default['private_chef']['postgresql']['log_directory'] = "/var/log/opscode/postgresql/#{node['private_chef']['postgresql']['version']}"
default['private_chef']['postgresql']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['postgresql']['log_rotation']['num_to_keep'] = 10
default['private_chef']['postgresql']['username'] = "opscode-pgsql"
default['private_chef']['postgresql']['shell'] = "/bin/sh"
default['private_chef']['postgresql']['home'] = "/var/opt/opscode/postgresql"
default['private_chef']['postgresql']['user_path'] = "/opt/opscode/embedded/bin:/opt/opscode/bin:$PATH"
default['private_chef']['postgresql']['sql_user'] = "opscode_chef"
default['private_chef']['postgresql']['sql_password'] = "snakepliskin"
default['private_chef']['postgresql']['sql_ro_user'] = "opscode_chef_ro"
default['private_chef']['postgresql']['sql_ro_password'] = "shmunzeltazzen"
default['private_chef']['postgresql']['vip'] = "127.0.0.1"
default['private_chef']['postgresql']['port'] = 5432
default['private_chef']['postgresql']['listen_address'] = 'localhost'
default['private_chef']['postgresql']['max_connections'] = 350
default['private_chef']['postgresql']['md5_auth_cidr_addresses'] = [ ]
default['private_chef']['postgresql']['trust_auth_cidr_addresses'] = [ '127.0.0.1/32', '::1/128' ]
default['private_chef']['postgresql']['shmmax'] = 17179869184
default['private_chef']['postgresql']['shmall'] = 4194304
default['private_chef']['postgresql']['shared_buffers'] = "#{(node['memory']['total'].to_i / 4) / (1024)}MB"
default['private_chef']['postgresql']['work_mem'] = "8MB"
default['private_chef']['postgresql']['effective_cache_size'] = "128MB"
default['private_chef']['postgresql']['checkpoint_segments'] = 3
default['private_chef']['postgresql']['checkpoint_timeout'] = "5min"
default['private_chef']['postgresql']['checkpoint_completion_target'] = 0.5
default['private_chef']['postgresql']['checkpoint_warning'] = "30s"

###
# Bifrost
###
default['private_chef']['oc_bifrost']['enable'] = true
default['private_chef']['oc_bifrost']['ha'] = false
default['private_chef']['oc_bifrost']['dir'] = "/var/opt/opscode/oc_bifrost"
default['private_chef']['oc_bifrost']['log_directory'] = "/var/log/opscode/oc_bifrost"
default['private_chef']['oc_bifrost']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['oc_bifrost']['log_rotation']['num_to_keep'] = 10
default['private_chef']['oc_bifrost']['vip'] = '127.0.0.1'
default['private_chef']['oc_bifrost']['listen'] = '127.0.0.1'
default['private_chef']['oc_bifrost']['port'] = 9463
default['private_chef']['oc_bifrost']['superuser_id'] = '5ca1ab1ef005ba111abe11eddecafbad'
default['private_chef']['oc_bifrost']['db_pool_size'] = '20'
default['private_chef']['oc_bifrost']['sql_user'] = "bifrost"
default['private_chef']['oc_bifrost']['sql_password'] = "challengeaccepted"
default['private_chef']['oc_bifrost']['sql_ro_user'] = "bifrost_ro"
default['private_chef']['oc_bifrost']['sql_ro_password'] = "foreveralone"
# Enable extended performance logging data for bifrost.  Setting this to false
# will cut bifrost request log size approximately in half.
default['private_chef']['oc_bifrost']['extended_perf_log'] = true

####
# Authz
####
default['private_chef']['oc_chef_authz']['http_init_count'] = 25
default['private_chef']['oc_chef_authz']['http_max_count'] = 100
default['private_chef']['oc_chef_authz']['http_cull_interval'] = "{1, min}"
default['private_chef']['oc_chef_authz']['http_max_age'] = "{70, sec}"
default['private_chef']['oc_chef_authz']['http_max_connection_duration'] = "{70, sec}"
default['private_chef']['oc_chef_authz']['ibrowse_options'] = "[{connect_timeout, 5000}]"

####
# Bookshelf
####
default['private_chef']['bookshelf']['enable'] = true
default['private_chef']['bookshelf']['ha'] = false
default['private_chef']['bookshelf']['dir'] = "/var/opt/opscode/bookshelf"
default['private_chef']['bookshelf']['data_dir'] = "/var/opt/opscode/bookshelf/data"
default['private_chef']['bookshelf']['log_directory'] = "/var/log/opscode/bookshelf"
default['private_chef']['bookshelf']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['bookshelf']['log_rotation']['num_to_keep'] = 10
default['private_chef']['bookshelf']['vip'] = node['private_chef']['lb']['api_fqdn']
default['private_chef']['bookshelf']['listen'] = '127.0.0.1'
default['private_chef']['bookshelf']['port'] = 4321
default['private_chef']['bookshelf']['stream_download'] = true
default['private_chef']['bookshelf']['access_key_id'] = "generated-by-default"
default['private_chef']['bookshelf']['secret_access_key'] = "generated-by-default"
# Default: set to Host: header. Override to hardcode a url, "http://..."
default['private_chef']['bookshelf']['external_url'] = :host_header

###
# Opscode Certificate
###
default['private_chef']['opscode-certificate']['enable'] = true
default['private_chef']['opscode-certificate']['ha'] = false
default['private_chef']['opscode-certificate']['dir'] = "/var/opt/opscode/opscode-certificate"
default['private_chef']['opscode-certificate']['log_directory'] = "/var/log/opscode/opscode-certificate"
default['private_chef']['opscode-certificate']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['opscode-certificate']['log_rotation']['num_to_keep'] = 10
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
default['private_chef']['opscode-org-creator']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['opscode-org-creator']['log_rotation']['num_to_keep'] = 10
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
default['private_chef']['dark_launch']["add_type_and_bag_to_items"] = true
default['private_chef']['dark_launch']["reporting"] = true
default['private_chef']['dark_launch']["actions"] = false

###
# Opscode Account
###
default['private_chef']['opscode-account']['enable'] = true
default['private_chef']['opscode-account']['ha'] = false
default['private_chef']['opscode-account']['dir'] = "/var/opt/opscode/opscode-account"
default['private_chef']['opscode-account']['log_directory'] = "/var/log/opscode/opscode-account"
default['private_chef']['opscode-account']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['opscode-account']['log_rotation']['num_to_keep'] = 10
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
# Chef Mover
###
default['private_chef']['opscode-chef-mover']['enable'] = true
default['private_chef']['opscode-chef-mover']['ha'] = false
default['private_chef']['opscode-chef-mover']['dir'] = "/var/opt/opscode/opscode-chef-mover"
default['private_chef']['opscode-chef-mover']['data_dir'] = "/var/opt/opscode/opscode-chef-mover/data"
default['private_chef']['opscode-chef-mover']['log_directory'] = "/var/log/opscode/opscode-chef-mover"
default['private_chef']['opscode-chef-mover']['log_rotation']['file_maxbytes'] = 1073741824
default['private_chef']['opscode-chef-mover']['log_rotation']['num_to_keep'] = 10
default['private_chef']['opscode-chef-mover']['bulk_fetch_batch_size'] = '5'
default['private_chef']['opscode-chef-mover']['max_cache_size'] = '10000'
default['private_chef']['opscode-chef-mover']['cache_ttl'] = '3600'
default['private_chef']['opscode-chef-mover']['db_pool_size'] = '5'
default['private_chef']['opscode-chef-mover']['ibrowse_max_sessions'] = 256
default['private_chef']['opscode-chef-mover']['ibrowse_max_pipeline_size'] = 1
default['private_chef']['opscode-chef-mover']['solr_timeout'] = 30000
default['private_chef']['opscode-chef-mover']['solr_http_init_count'] = 25
default['private_chef']['opscode-chef-mover']['solr_http_max_count'] = 100
default['private_chef']['opscode-chef-mover']['solr_http_cull_interval'] = "{1, min}"
default['private_chef']['opscode-chef-mover']['solr_http_max_age'] = "{70, sec}"
default['private_chef']['opscode-chef-mover']['solr_http_max_connection_duration'] = "{70,sec}"
default['private_chef']['opscode-chef-mover']['solr_ibrowse_options'] = "[{connect_timeout, 10000}]"


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
default['private_chef']['keepalived']['ipv6_on'] = false
default['private_chef']['keepalived']['log_directory'] = "/var/log/opscode/keepalived"
default['private_chef']['keepalived']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['keepalived']['log_rotation']['num_to_keep'] = 10
default['private_chef']['keepalived']['smtp_server'] = "127.0.0.1"
default['private_chef']['keepalived']['smtp_connect_timeout'] = "30"
default['private_chef']['keepalived']['vrrp_sync_group'] = "PC_GROUP"
default['private_chef']['keepalived']['vrrp_sync_instance'] = "PC_VI"
default['private_chef']['keepalived']['vrrp_instance_state'] = "BACKUP"
default['private_chef']['keepalived']['vrrp_instance_interface'] = "eth0"
default['private_chef']['keepalived']['vrrp_instance_virtual_router_id'] = "1"
default['private_chef']['keepalived']['vrrp_instance_priority'] = "100"
default['private_chef']['keepalived']['vrrp_instance_advert_int'] = "1"
default['private_chef']['keepalived']['vrrp_instance_password'] = "sneakybeaky"
default['private_chef']['keepalived']['vrrp_instance_ipaddress'] = node['ipaddress']
default['private_chef']['keepalived']['vrrp_instance_ipaddress_dev'] = 'eth0'
default['private_chef']['keepalived']['vrrp_instance_vrrp_unicast_bind'] = node['ipaddress']
default['private_chef']['keepalived']['vrrp_instance_vrrp_unicast_peer'] = nil
default['private_chef']['keepalived']['vrrp_instance_preempt_delay'] = 30
default['private_chef']['keepalived']['vrrp_instance_nopreempt'] = true

default['private_chef']['keepalived']['service_posthooks'] = {
    "rabbitmq" => "/opt/opscode/bin/wait-for-rabbit"
}

##
# LDAP Authentication Integration
##
default['private_chef']['ldap'] = nil

##
# Upgrades/Partybus
##
default['private_chef']['upgrades']['dir'] = "/var/opt/opscode/upgrades"
