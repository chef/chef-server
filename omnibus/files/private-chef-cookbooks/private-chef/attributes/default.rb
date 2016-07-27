# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

KB_IN_GIG = 1048576

###
# Set a project-name for the enterprise-chef-common cookbook
###
default['enterprise']['name'] = "private_chef"

default['private_chef']['removed_services'] = %w{
opscode-webui
opscode-solr
couchdb
opscode-account
opscode-org-creator
opscode-certificate
}

default['private_chef']['hidden_services'] = %w{
opscode-chef-mover
}

###
# High level options
###
default['private_chef']['api_version'] = "12.0.0"
default['private_chef']['flavor'] = "cs"
default['private_chef']['install_path'] = "/opt/opscode"

default['private_chef']['notification_email'] = "pc-default@chef.io"
default['private_chef']['from_email'] = '"Opscode" <donotreply@chef.io>'
default['private_chef']['role'] = "standalone"
default['private_chef']['license']['nodes'] = 25
default['private_chef']['license']['upgrade_url'] = "http://www.chef.io/contact/on-premises-simple"

default['private_chef']['default_orgname'] = nil

# Enable fips mode (openssl)
# This requires the chef-server-fips package. If you do not have this,
# your chef-server will probably not work. If you have to manually
# change this, you're doing it wrong.
default['private_chef']['fips_enabled'] = false

###
# Options for installing addons
###
default['private_chef']['addons']['install'] = false
default['private_chef']['addons']['path'] = nil
default['private_chef']['addons']['packages'] =
  %w{opscode-reporting opscode-manage opscode-analytics opscode-push-jobs-server chef-ha}
default['private_chef']['addons']['ubuntu_supported_codenames'] =
  %w{lucid precise trusty}
default['private_chef']['addons']['ubuntu_distribution'] =
  node['private_chef']['addons']['ubuntu_supported_codenames'].include?(node['lsb']['codename']) ?
  node['lsb']['codename'] : 'lucid'

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
# Service data_dir, etc_dir, log_directory perms
####
default['private_chef']['service_dir_perms'] = "0750"

####
# CouchDB (legacy required for upgrades to work)
####
default['private_chef']['couchdb']['data_dir'] = "/var/opt/opscode/couchdb/db"
default['private_chef']['couchdb']['vip'] = "127.0.0.1"
default['private_chef']['couchdb']['port'] = 5984

####
# Opscode Solr (legacy required for upgrade cleanup to work)
####
default['private_chef']['opscode-solr']['data_dir'] = "/var/opt/opscode/opscode-solr/data"

####
# Server API Version - is not used in server configuration, but rather in the configuration
# of components that need to know how to talk to the erchef server.
#
# This is set to the current minimally supported version.
####
default['private_chef']['server-api-version'] = 0

####
# HAProxy
#
# HAProxy is only used when use_chef_backend is true. All Postgresql
# and Elasticsearch requests are routed to it and then forwarded to
# the current chef-backend leader.
####
default['private_chef']['haproxy']['enable'] = true
default['private_chef']['haproxy']['ha'] = false
default['private_chef']['haproxy']['dir'] = "/var/opt/opscode/haproxy"
default['private_chef']['haproxy']['log_directory'] = "/var/log/opscode/haproxy"
default['private_chef']['haproxy']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['haproxy']['log_rotation']['num_to_keep'] = 10
default['private_chef']['haproxy']['listen'] = '0.0.0.0'
default['private_chef']['haproxy']['local_postgresql_port'] = 5432
default['private_chef']['haproxy']['remote_postgresql_port'] = 5432
default['private_chef']['haproxy']['local_elasticsearch_port'] = 9200
default['private_chef']['haproxy']['remote_elasticsearch_port'] = 9200
default['private_chef']['haproxy']['leaderl_healthcheck_port'] = 7331
default['private_chef']['haproxy']['etcd_port'] = 2379

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
default['private_chef']['rabbitmq']['actions_user'] = 'actions'
default['private_chef']['rabbitmq']['actions_password'] = 'changeme'
default['private_chef']['rabbitmq']['actions_vhost'] = '/analytics'
default['private_chef']['rabbitmq']['actions_exchange'] = 'actions'
default['private_chef']['rabbitmq']['node_ip_address'] = '127.0.0.1'
default['private_chef']['rabbitmq']['node_port'] = '5672'
default['private_chef']['rabbitmq']['nodename'] = 'rabbit@localhost'
default['private_chef']['rabbitmq']['vip'] = '127.0.0.1'
default['private_chef']['rabbitmq']['consumer_id'] = 'hotsauce'
default['private_chef']['rabbitmq']['env_path'] = "/opt/opscode/bin:/opt/opscode/embedded/bin:/usr/bin:/bin"

default['private_chef']['rabbitmq']['ssl_versions'] = ['tlsv1.2', 'tlsv1.1']

####
# RabbitMQ Management Plugin
####
default['private_chef']['rabbitmq']['management_user'] = "rabbitmgmt"
default['private_chef']['rabbitmq']['management_password'] = "chefrocks"
default['private_chef']['rabbitmq']['management_port'] = 15672
default['private_chef']['rabbitmq']['management_enabled'] = true

# RabbitMQ max-length policy
default['private_chef']['rabbitmq']['analytics_max_length'] = 10000
default['private_chef']['rabbitmq']['queue_length_monitor_vhost'] = "/analytics"
default['private_chef']['rabbitmq']['queue_length_monitor_queue'] = "alaska"
default['private_chef']['rabbitmq']['queue_length_monitor_enabled'] = true
# does a full queue set overall_status to fail at \_status
default['private_chef']['rabbitmq']['queue_at_capacity_affects_overall_status'] = false

####
# RabbitMQ Queue Monitor
####
# how often to run the queue monitor
default['private_chef']['rabbitmq']['queue_length_monitor_millis'] = 30000
# if the queue monitor is busy and this timeout has been exceeded,
# assume that rabbit is in a bad state and don't send messages to it
# 5000 is the default of gen_server:call()
default['private_chef']['rabbitmq']['queue_length_monitor_timeout_millis'] = 5000

# don't send messages to rabbitmq if it has reached it's configured max_length
default['private_chef']['rabbitmq']['drop_on_full_capacity'] = true

# prevent erchef from starting if queue is at capacity
default['private_chef']['rabbitmq']['prevent_erchef_startup_on_full_capacity'] = false

# rabbit_mgmt_service configuration for erchef. These are used to configure an opscoderl_httpc pool
# of HTTP connecton workers.
default['private_chef']['rabbitmq']['rabbit_mgmt_timeout'] = 30000
default['private_chef']['rabbitmq']['rabbit_mgmt_http_init_count'] = 25
default['private_chef']['rabbitmq']['rabbit_mgmt_http_max_count'] = 100
# cull interval specified in seconds
default['private_chef']['rabbitmq']['rabbit_mgmt_http_cull_interval'] = 60
# max age specified in seconds
default['private_chef']['rabbitmq']['rabbit_mgmt_http_max_age'] = 70
#max connection duration specified in seconds
default['private_chef']['rabbitmq']['rabbit_mgmt_http_max_connection_duration'] = 70

# comma sep list of tuples, without surrounding []'s
# rendered as a list in oc_erchef.config.erb, including basic_auth info
default['private_chef']['rabbitmq']['rabbit_mgmt_ibrowse_options'] =  "{connect_timeout, 10000}"

####
# External RabbitMQ
####
default['private_chef']['external-rabbitmq']['enable'] = false
default['private_chef']['external-rabbitmq']['actions_user'] = 'actions'
default['private_chef']['external-rabbitmq']['actions_password'] = 'changeme'
default['private_chef']['external-rabbitmq']['actions_vhost'] = '/analytics'
default['private_chef']['external-rabbitmq']['actions_exchange'] = 'actions'
default['private_chef']['external-rabbitmq']['node_port'] = '5672'
default['private_chef']['external-rabbitmq']['vip'] = '127.0.0.1'

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
#
# Set this to point at a solr/cloudsearch installation
# not controlled by chef-server
#
default['private_chef']['opscode-solr4']['external'] = false
default['private_chef']['opscode-solr4']['external_url'] = nil
default['private_chef']['opscode-solr4']['ha'] = false
default['private_chef']['opscode-solr4']['dir'] = "/var/opt/opscode/opscode-solr4"
default['private_chef']['opscode-solr4']['data_dir'] = "/var/opt/opscode/opscode-solr4/data"
default['private_chef']['opscode-solr4']['temp_directory'] = "/var/opt/opscode/opscode-solr4/"
default['private_chef']['opscode-solr4']['log_directory'] = "/var/log/opscode/opscode-solr4"
default['private_chef']['opscode-solr4']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['opscode-solr4']['log_rotation']['num_to_keep'] = 10
# defaults for heap size and new generation size are computed in the chef-solr
# recipe based on node memory
default['private_chef']['opscode-solr4']['heap_size'] = nil
default['private_chef']['opscode-solr4']['new_size'] = nil
default['private_chef']['opscode-solr4']['java_opts'] = ""
default['private_chef']['opscode-solr4']['url'] = "http://localhost:8983/solr"
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
default['private_chef']['opscode-erchef']['log_rotation']['max_messages_per_second'] = 1000
default['private_chef']['opscode-erchef']['vip'] = '127.0.0.1'
default['private_chef']['opscode-erchef']['listen'] = '127.0.0.1'
default['private_chef']['opscode-erchef']['port'] = 8000
default['private_chef']['opscode-erchef']['auth_skew'] = '900'
default['private_chef']['opscode-erchef']['authz_pooler_timeout'] = '0'
default['private_chef']['opscode-erchef']['bulk_fetch_batch_size'] = '5'
default['private_chef']['opscode-erchef']['udp_socket_pool_size'] = '20'
default['private_chef']['opscode-erchef']['sql_user'] = "opscode_chef"
default['private_chef']['opscode-erchef']['sql_password'] = "snakepliskin"
default['private_chef']['opscode-erchef']['sql_ro_user'] = "opscode_chef_ro"
default['private_chef']['opscode-erchef']['sql_ro_password'] = "shmunzeltazzen"
# Pool configuration for postgresql connections
#
# db_pool_size - the number of pgsql connections in the pool
#
# db_pool_queue_max - the maximum number of pgsql requests to queue up
# if all connections are busy
#
# db_pooler_timeout - the maximum amount of time a request should wait
# in the queue before timing out.  Request queueing is only effective
# if db_pooler_timeout > 0
default['private_chef']['opscode-erchef']['db_pool_size'] = 20
default['private_chef']['opscode-erchef']['db_pool_max'] = nil
default['private_chef']['opscode-erchef']['db_pool_init'] = nil
default['private_chef']['opscode-erchef']['db_pool_queue_max'] = 20
default['private_chef']['opscode-erchef']['db_pooler_timeout'] = 2000
default['private_chef']['opscode-erchef']['sql_db_timeout'] = 5000

# Pool configuration for depsolver workers
#
# depsolver_worker_count - the number of depselector workers.  This is
# a CPU bound task, thus setting this over the number of CPUs is not
# advised.
#
# depsolver_pool_queue_max - the number of depsolve requets to queue
# if all workers are busy.
#
# depsolver_pooler_timeout - the time in ms to wait before a queued
# request times out.  Requesting queueing is only effective if
# db_pooler_timeout > 0
#
# depsolver_tiemout - the amount of time to wait in ms before a
# request to a depsolver worker is abandoned.
default['private_chef']['opscode-erchef']['depsolver_pooler_timeout'] = '0'
default['private_chef']['opscode-erchef']['depsolver_pool_queue_max'] = '50'
default['private_chef']['opscode-erchef']['depsolver_worker_count'] = 5
default['private_chef']['opscode-erchef']['depsolver_timeout'] = 5000
default['private_chef']['opscode-erchef']['couchdb_max_conn'] = '100'
default['private_chef']['opscode-erchef']['ibrowse_max_sessions'] = 256
default['private_chef']['opscode-erchef']['ibrowse_max_pipeline_size'] = 1
# general search settings used to set up chef_index
default['private_chef']['opscode-erchef']['search_provider'] = "solr" # solr, elasticsearch
default['private_chef']['opscode-erchef']['search_queue_mode'] = "rabbitmq" # rabbitmq, batch, or inline
default['private_chef']['opscode-erchef']['search_batch_max_size'] = "5000000"
default['private_chef']['opscode-erchef']['search_batch_max_wait'] = "10"
# solr_service configuration for erchef. These are used to configure an opscoderl_httpc pool
# of HTTP connecton workers.
default['private_chef']['opscode-erchef']['solr_timeout'] = 30000
default['private_chef']['opscode-erchef']['solr_http_init_count'] = 25
default['private_chef']['opscode-erchef']['solr_http_max_count'] = 100
default['private_chef']['opscode-erchef']['solr_http_cull_interval'] = "{1, min}"
default['private_chef']['opscode-erchef']['solr_http_max_age'] = "{70, sec}"
default['private_chef']['opscode-erchef']['solr_http_max_connection_duration'] = "{70,sec}"
default['private_chef']['opscode-erchef']['solr_ibrowse_options'] = "[{connect_timeout, 10000}]"
# Default: generate signed URLs based upon Host: header. Override with a url, "http:// ..."
default['private_chef']['opscode-erchef']['base_resource_url'] = :host_header
default['private_chef']['opscode-erchef']['s3_bucket'] = 'bookshelf'
default['private_chef']['opscode-erchef']['s3_url_ttl'] = 28800
default['private_chef']['opscode-erchef']['nginx_bookshelf_caching'] = :off
default['private_chef']['opscode-erchef']['s3_url_expiry_window_size'] = :off
default['private_chef']['opscode-erchef']['s3_parallel_ops_timeout'] = 5000
default['private_chef']['opscode-erchef']['s3_parallel_ops_fanout'] = 20
default['private_chef']['opscode-erchef']['authz_timeout'] = 2000
default['private_chef']['opscode-erchef']['authz_fanout'] = 20
default['private_chef']['opscode-erchef']['root_metric_key'] = "chefAPI"
default['private_chef']['opscode-erchef']['max_request_size'] = 1000000
default['private_chef']['opscode-erchef']['cleanup_batch_size'] = 0
default['private_chef']['opscode-erchef']['keygen_cache_size'] = 10
default['private_chef']['opscode-erchef']['keygen_start_size'] = 0
default['private_chef']['opscode-erchef']['keygen_cache_workers'] = :auto
default['private_chef']['opscode-erchef']['keygen_timeout'] = 1000
default['private_chef']['opscode-erchef']['keygen_key_size'] = 2048
default['private_chef']['opscode-erchef']['strict_search_result_acls'] = false
default['private_chef']['opscode-erchef']['ssl_session_caching']['enabled'] = false

# The amount of milliseconds before we timeout and assume an endpoint is down for
# the /_status endpoint.

default['private_chef']['opscode-erchef']['health_ping_timeout'] = 400

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
default['private_chef']['lb']['access_by_lua_file'] = false

###
# Load balancer route configuration
###
default['private_chef']['lb']['xdl_defaults']['503_mode'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_containers'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_groups'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_acls'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_association_requests'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_organizations'] = false
default['private_chef']['lb']['xdl_defaults']['couchdb_associations'] = false

####
# Nginx
####
default['private_chef']['nginx']['enable'] = true
default['private_chef']['nginx']['ha'] = false
default['private_chef']['nginx']['dir'] = "/var/opt/opscode/nginx"
default['private_chef']['nginx']['log_directory'] = "/var/log/opscode/nginx"
default['private_chef']['nginx']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['nginx']['log_rotation']['num_to_keep'] = 10
default['private_chef']['nginx']['log_x_forwarded_for'] = false
default['private_chef']['nginx']['ssl_port'] = 443
default['private_chef']['nginx']['enable_non_ssl'] = false
default['private_chef']['nginx']['non_ssl_port'] = 80
default['private_chef']['nginx']['x_forwarded_proto'] = 'https'
default['private_chef']['nginx']['server_name'] = node['fqdn']
default['private_chef']['nginx']['url'] = "https://#{node['fqdn']}"
default['private_chef']['nginx']['proxy_connect_timeout'] = 1
# Support for the stub_status module
default['private_chef']['nginx']['enable_stub_status'] = true
default['private_chef']['nginx']['stub_status']['listen_host'] = "127.0.0.1"
default['private_chef']['nginx']['stub_status']['listen_port'] = "9999"
default['private_chef']['nginx']['stub_status']['location'] = "/nginx_status"
default['private_chef']['nginx']['stub_status']['allow_list'] = ["127.0.0.1"]
# Based off of the Mozilla recommended cipher suite
# https://wiki.mozilla.org/Security/Server_Side_TLS#Recommended_Ciphersuite
#
# SSLV3 was removed because of the poodle attack. (https://www.openssl.org/~bodo/ssl-poodle.pdf)
#
# If your infrastructure still has requirements for the vulnerable/venerable SSLV3, you can add
# "SSLv3" to the below line.
default['private_chef']['nginx']['ssl_protocols'] = "TLSv1 TLSv1.1 TLSv1.2"
default['private_chef']['nginx']['ssl_ciphers'] = "ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA"
#
# The SSL Certificate and DH Param will be automatically generated if
# these are nil.  Otherwise we expect these attributes to point at the
# on-disk location of a user-provided certificate and dhparam
default['private_chef']['nginx']['ssl_certificate'] = nil
default['private_chef']['nginx']['ssl_certificate_key'] = nil
default['private_chef']['nginx']['ssl_dhparam'] = nil
default['private_chef']['nginx']['ssl_country_name'] = "US"
default['private_chef']['nginx']['ssl_company_name'] = "YouCorp"
default['private_chef']['nginx']['ssl_organizational_unit_name'] = "Operations"
default['private_chef']['nginx']['ssl_key_length'] = 2048
default['private_chef']['nginx']['ssl_duration'] = 3650
default['private_chef']['nginx']['dhparam_key_length'] = 2048
default['private_chef']['nginx']['dhparam_generator_id'] = 2
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
default['private_chef']['nginx']['server_names_hash_bucket_size'] = 128
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
default['private_chef']['postgresql']['external'] = false
default['private_chef']['postgresql']['ha'] = false
default['private_chef']['postgresql']['dir'] = "/var/opt/opscode/postgresql/#{node['private_chef']['postgresql']['version']}"
default['private_chef']['postgresql']['data_dir'] = "/var/opt/opscode/postgresql/#{node['private_chef']['postgresql']['version']}/data"
default['private_chef']['postgresql']['log_directory'] = "/var/log/opscode/postgresql/#{node['private_chef']['postgresql']['version']}"
default['private_chef']['postgresql']['log_min_duration_statement'] = -1
default['private_chef']['postgresql']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['postgresql']['log_rotation']['num_to_keep'] = 10
default['private_chef']['postgresql']['username'] = "opscode-pgsql"
default['private_chef']['postgresql']['db_superuser'] = 'opscode-pgsql'
default['private_chef']['postgresql']['db_superuser_password'] = 'doallthethings'
default['private_chef']['postgresql']['shell'] = "/bin/sh"
default['private_chef']['postgresql']['home'] = "/var/opt/opscode/postgresql"
default['private_chef']['postgresql']['user_path'] = "/opt/opscode/embedded/bin:/opt/opscode/bin:$PATH"
default['private_chef']['postgresql']['vip'] = "127.0.0.1"
default['private_chef']['postgresql']['port'] = 5432
default['private_chef']['postgresql']['listen_address'] = 'localhost'
default['private_chef']['postgresql']['max_connections'] = 350
default['private_chef']['postgresql']['keepalives_idle'] = 60
default['private_chef']['postgresql']['keepalives_interval'] = 15
default['private_chef']['postgresql']['keepalives_count'] = 2
default['private_chef']['postgresql']['md5_auth_cidr_addresses'] = [ '127.0.0.1/32', '::1/128' ]
default['private_chef']['postgresql']['shmmax'] = 17179869184
default['private_chef']['postgresql']['shmall'] = 4194304
default['private_chef']['postgresql']['wal_level'] = "minimal"
default['private_chef']['postgresql']['archive_mode'] = "off"
default['private_chef']['postgresql']['archive_command'] = ""
default['private_chef']['postgresql']['archive_timeout'] = 0 # 0 is disabled.

# Make sure we don't allocate more shared memory than the max.
# Especailly relevant on large machines. #597
#
# This is based on the tuning parameters here:
#  https://wiki.postgresql.org/wiki/Tuning_Your_PostgreSQL_Server
# They generally recomend 25% of avalable memory, and we reserve 15% of shared memory for other uses
# We limit to 2GB less than shmmax as exessive amounts of shared memory is unhelpful.

# Convert everything to KB to avoid unit confusion
shmkb = node['private_chef']['postgresql']['shmmax'] / 1024
quarter_mem = node['memory']['total'].to_i/4
postgres_max_mem = [0.85*shmkb, shmkb - 2*KB_IN_GIG].min
if(quarter_mem > postgres_max_mem)
  shared_bytes = postgres_max_mem
else
  shared_bytes = quarter_mem
end
default['private_chef']['postgresql']['shared_buffers'] = "#{(shared_bytes/1024).to_i}MB"

default['private_chef']['postgresql']['work_mem'] = "8MB"
default['private_chef']['postgresql']['effective_cache_size'] = "#{(node['memory']['total'].to_i / 2) / (1024)}MB"
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
default['private_chef']['oc_bifrost']['log_rotation']['max_messages_per_second'] = 1000
default['private_chef']['oc_bifrost']['vip'] = '127.0.0.1'
default['private_chef']['oc_bifrost']['listen'] = '127.0.0.1'
default['private_chef']['oc_bifrost']['port'] = 9463
default['private_chef']['oc_bifrost']['superuser_id'] = '5ca1ab1ef005ba111abe11eddecafbad'
default['private_chef']['oc_bifrost']['db_pool_size'] = '20'
default['private_chef']['oc_bifrost']['db_pool_max'] = nil
default['private_chef']['oc_bifrost']['db_pool_init'] = nil
# The db_pool is only effective for a db_pooler_timeout > 0
default['private_chef']['oc_bifrost']['db_pooler_timeout'] = 2000
default['private_chef']['oc_bifrost']['db_pool_queue_max'] = 20
default['private_chef']['oc_bifrost']['sql_user'] = "bifrost"
default['private_chef']['oc_bifrost']['sql_password'] = "challengeaccepted"
default['private_chef']['oc_bifrost']['sql_ro_user'] = "bifrost_ro"
default['private_chef']['oc_bifrost']['sql_ro_password'] = "foreveralone"
default['private_chef']['oc_bifrost']['sql_db_timeout'] = 5000
# Enable extended performance logging data for bifrost.  Setting this to false
# will cut bifrost request log size approximately in half.
default['private_chef']['oc_bifrost']['extended_perf_log'] = true

####
# Authz
####
default['private_chef']['oc_chef_authz']['http_init_count'] = 25
default['private_chef']['oc_chef_authz']['http_max_count'] = 100
# The queue max is only effective if authz_pooler_timeout (in the
# opscode-erchef configurables above) is > 0
default['private_chef']['oc_chef_authz']['http_queue_max'] = 50
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
default['private_chef']['bookshelf']['vip_port'] = 443
default['private_chef']['bookshelf']['listen'] = '127.0.0.1'
default['private_chef']['bookshelf']['port'] = 4321
default['private_chef']['bookshelf']['stream_download'] = true
default['private_chef']['bookshelf']['access_key_id'] = "generated-by-default"
default['private_chef']['bookshelf']['secret_access_key'] = "generated-by-default"
# Default: set to Host: header. Override to hardcode a url, "http://..."
default['private_chef']['bookshelf']['external_url'] = :host_header
default['private_chef']['bookshelf']['storage_type'] = :filesystem
# This retries connections that are rejected because pooler queue is maxed out.
default['private_chef']['bookshelf']['sql_retry_count'] = 0
# Intervals are in milliseconds
default['private_chef']['bookshelf']['sql_retry_delay'] = 10
default['private_chef']['bookshelf']['abandoned_upload_cleanup_interval'] = 19 * (60 * 1000)
default['private_chef']['bookshelf']['deleted_data_cleanup_interval'] = 7 * (60 * 1000)
default['private_chef']['bookshelf']['db_pool_size'] = 20
default['private_chef']['bookshelf']['db_pool_queue_max'] = 200
default['private_chef']['bookshelf']['db_pooler_timeout'] = 2000
default['private_chef']['bookshelf']['sql_db_timeout'] = 5000
default['private_chef']['bookshelf']['sql_ro_user'] = 'bookshelf_ro'
default['private_chef']['bookshelf']['sql_ro_password'] = 'should_never_be_used'
default['private_chef']['bookshelf']['sql_user'] = 'bookshelf'
default['private_chef']['bookshelf']['sql_password'] = 'should_never_be_used'

###
# Chef Identity
###

default['private_chef']['oc_id']['enable'] = true
default['private_chef']['oc_id']['ha'] = false
default['private_chef']['oc_id']['dir'] = "/var/opt/opscode/oc_id"
default['private_chef']['oc_id']['log_directory'] = "/var/log/opscode/oc_id"
default['private_chef']['oc_id']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['oc_id']['log_rotation']['num_to_keep'] = 10
default['private_chef']['oc_id']['vip'] = "127.0.0.1"
default['private_chef']['oc_id']['port'] = 9090
default['private_chef']['oc_id']['sql_database'] = "oc_id"
default['private_chef']['oc_id']['sql_user'] = "oc_id"
default['private_chef']['oc_id']['sql_password'] = "snakepliskin"
default['private_chef']['oc_id']['sql_ro_user'] = "oc_id_ro"
default['private_chef']['oc_id']['sql_ro_password'] = "look-but-don't-touch"
default['private_chef']['oc_id']['db_pool_size'] = '20'
default['private_chef']['oc_id']['sentry_dsn'] = nil
default['private_chef']['oc_id']['sign_up_url'] = nil

default['private_chef']['oc_id']['administrators'] = []

# Use to define predefined applications that can authenticate with the server.
# Entries are a hash with the key being the name of the application and the
# value being a hash with a 'redirect_uri' key. Example:
#
#     oc_id['applications'] = {
#       'supermarket' => {
#         'redirect_uri' => 'http://supermarket.mycorp/auth/chef_oauth2/callback'
#       },
#       'another_app' => {
#         'redirect_uri' => 'http://anotherapp.mycorp/auth/chef_oauth2/callback'
#       }
#     }
#
# Default value: `{}`.
default['private_chef']['oc_id']['applications'] = {}

###
# Dark Launch
###
default['private_chef']['dark_launch']["quick_start"] = false
default['private_chef']['dark_launch']["new_theme"] = true
default['private_chef']['dark_launch']["private-chef"] = true
default['private_chef']['dark_launch']["sql_users"] = true
default['private_chef']['dark_launch']["add_type_and_bag_to_items"] = true
default['private_chef']['dark_launch']["reporting"] = true
default['private_chef']['dark_launch']["actions"] = true

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
default['private_chef']['opscode-chef-mover']['sql_db_timeout'] = 5000
default['private_chef']['opscode-chef-mover']['ibrowse_max_sessions'] = 256
default['private_chef']['opscode-chef-mover']['ibrowse_max_pipeline_size'] = 1
default['private_chef']['opscode-chef-mover']['solr_timeout'] = 30000
default['private_chef']['opscode-chef-mover']['solr_http_init_count'] = 25
default['private_chef']['opscode-chef-mover']['solr_http_max_count'] = 100
default['private_chef']['opscode-chef-mover']['solr_http_cull_interval'] = "{1, min}"
default['private_chef']['opscode-chef-mover']['solr_http_max_age'] = "{70, sec}"
default['private_chef']['opscode-chef-mover']['solr_http_max_connection_duration'] = "{70,sec}"
default['private_chef']['opscode-chef-mover']['solr_ibrowse_options'] = "[{connect_timeout, 10000}]"
default['private_chef']['opscode-chef-mover']['bulk_fetch_batch_size'] = 5

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
default['private_chef']['estatsd']['protocol'] = "estatsd"

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
# default['private_chef']['ldap']['host'] = "ldap-server-host"
# default['private_chef']['ldap']['bind_dn'] = "CN=user_who_can_search,OU=Employees,OU=Domain users,DC=example,DC=com"
# default['private_chef']['ldap']['bind_password'] = "plaintext password for binding w/ bind_dn"
# default['private_chef']['ldap']['base_dn'] = "OU=Employees,OU=Domain users,DC=example,DC=com"
# default['private_chef']['ldap']['timeout'] = 60000
# default['private_chef']['ldap']['port'] = 389
## Nearly every attribute in the standard LDAP schema that users likely set login_attr
## to is case sensitive.
# default['private_chef']['ldap']['case_sensitive_login_attribute'] = false
#
# default['private_chef']['ldap']['enable_ssl'] = false
# default['private_chef']['ldap']['enable_tls'] = false

##
# Upgrades/Partybus
##
default['private_chef']['upgrades']['dir'] = "/var/opt/opscode/upgrades"

##
# Folsom Graphite
##
# Off by default, graphite not included with chef.
default['private_chef']['folsom_graphite']['enabled'] = false
# graphite server host name
default['private_chef']['folsom_graphite']['host'] = "localhost"
# graphite server port
default['private_chef']['folsom_graphite']['port'] = 8080
# graphite prefix for the erchef stats
default['private_chef']['folsom_graphite']['prefix'] = "stats_prefix"
# how frequently to send stats to the graphite server in milliseconds
default['private_chef']['folsom_graphite']['send_interval'] = 10000
# if a connection fails, how frequently do we attempt to reconnect?
default['private_chef']['folsom_graphite']['retry_interval'] = 2000

#
# Data Collector
#
# data_collector configuration for erchef. These are used to configure an
# opscoderl_httpc pool of HTTP connecton workers.
# If a root_url and token are present the erchef will start the data_collector
# application. If proxy and root_url are present, nginx will send data_collector
# events to the insights server

# Proxy events to the insights data collector
# default['private_chef']['data_collector']['proxy']

# Fully qualified URL to the data collector server (e.g.: https://localhost/insights).
# default['private_chef']['data_collector']['root_url']
# The authentication token to pass via the header to the data collector server
# default['private_chef']['data_collector']['token']
# Timeout for requests to the data collector server in milliseconds.
default['private_chef']['data_collector']['timeout'] = 30000
# How many HTTP workers to start in the pool.
default['private_chef']['data_collector']['http_init_count'] = 25
# Maximum number of HTTP workers in the pool.
default['private_chef']['data_collector']['http_max_count'] = 100
# Maximum age of a server pool worker before terminating it.
default['private_chef']['data_collector']['http_max_age'] = "{70, sec}"
# How often to cull aged-out connections.
default['private_chef']['data_collector']['http_cull_interval'] = "{1, min}"
# Maximum age of a connection before terminating it.
default['private_chef']['data_collector']['http_max_connection_duration'] = "{70,sec}"
# Options for the ibrowse connections (see ibrowse).
default['private_chef']['data_collector']['ibrowse_options'] = "[{connect_timeout, 10000}]"
