# Enterprise Chef Changelog

## 12.0.0 (Unreleased)

## rabbitmq 3.3.4
* Upgrade to RabbitMQ 3.3.4

### oc-chef-pedant 1.0.53
* Remove /system-recovery endpoint tests

### postgresql 2014.07.29
* [OC-11672] Upgrade PostgreSQL to 9.2.9

### enterprise-chef-common 2014.07.21
* [OC-11575] Don't start services by default in HA topology
* Update to 0.4.4

### ruby updated to 1.9.3-p547
* Update is from 1.9.3-p484

### oc_erchef updated to 0.25.18
* Replace usage of cert generation service with erchef internal key
  generation cache.

### oc_erchef updated to 0.25.17
* Add global placeholder org macro.
* System recovery endpoint work: Fix so recovery_authentication_enabled is correct for new users

### oc_erchef updated to 0.25.16
* Bugfix to not force user key type on regeneration

### oc_erchef updated to 0.25.15
* Bugfix for concurrent cookbook uploads

### oc_erchef updated to 0.25.14
* Update to latest of oc_chef_action to get hostname from fqdn instead
  of inet
* Setting the CHEF_ACTIONS_MESSAGE_VERSION to 0.1.0

### cacerts 2014.04.22
* Update to latest cacerts as of 2014-04-22

### chef 11.12.2
* Update embedded chef gem to 11.12.2

### chef-mover 2.2.4
* Containers and groups migration from couchDB to postgreSQL
* Solr4 migration
* Generalized migrate scripts and other code to be migration_type agnostic

### enterprise-chef-server-schema 2.3.0
* Updates org_migration_state table with migration_type and verification
* Cleans up reporting schema info table
* Clean up Makefile to preserve PATH variable
* Add tables for organizations, org_user_associations, and org_user_invites

### logrotate
* Fixed build configuration that was causing logrotate to not be installed

### openssl 1.0.1i
* Fix for CVE-2014-3512
* Fix for CVE-2014-3511
* Fix for CVE-2014-3510
* Fix for CVE-2014-3507
* Fix for CVE-2014-3506
* Fix for CVE-2014-3505
* Fix for CVE-2014-3509
* Fix for CVE-2014-5139
* Fix for CVE-2014-3508

### opscode-account rel-1.51.0
* [OC-11702] - fails to expand ACLs and groups when they contain
  groups that no longer exist
* [OC-11708] - fixes user association bug that relied on permissions
  of the last updater of the users group

### opscode-webui
* Removed. WebUI usage should now come via the Manage extension.

### private-chef-administration
* Removed. Docs can be found at docs.opscode.com

### private-chef-cookbooks
* Allow ['lb']['upstream'] to have a custom setting
* Use SSL port for lb_internal if non-SSL is disabled
* Restart redis_lb immediately during reconfigure
* Separate redis_keepalive_timeout from redis_connection_timeout and
  increase their default values from 60ms to 1000 and 2000ms,
  respectively.
* [OC-11581] private-chef-ctl test command should return the pedant
  return code
* [OC-11499] Use more strict regular expression for IP check in ha-status
* [OC-3107] Ensure CouchDB compaction cron job does not run on passive
  backend.
* Introduce pluggable HA architecture as an alternative to DRBD
* [OC-11673] Tune PostgreSQL keepalive timeouts
* [OC-10117] opscode-solr4 accepts Java-like memory attributes
* [OC-11668] enable ipv6 in standalone mode
* [OC-11710] Fix couchdb compaction log rotation
* [OC-11297] tweak partybus migration-level subscribes for a more reliable workaround
* Add bifrost_sql_database uri to orgmapper.conf

### opscode-platform-debug
* Upgrade to rel-0.5.1

### private-chef-ctl

* Add a gather-logs command to create a tarball of important logs and
  system information for Chef Support

### openssl 1.0.1h
* Allow ['lb']['upstream'] to have a custom setting
* Use SSL port for lb_internal if non-SSL is disabled
* Restart redis_lb immediately during reconfigure

### oc-id 0.3.3
* Add Chef Identity Service (oc-id)

## 11.1.6 (2014-06-05)

### openssl 1.0.1h
* Address vulnerabilities CVE-2014-0224, CVE-2014-0221, CVE-2014-0195,
  CVE-2014-3470 https://www.openssl.org/news/secadv_20140605.txt

## 11.1.5 (2014-05-14)

### oc_erchef 0.24.6
* rename oc_actionlog to actions

## private-chef-cookbooks
* Use dark launch to enable Chef Actions (default: off)
* Write out Actions configuration file for use by opscode-analytics

## 11.1.4 (2014-05-07)

### oc-chef-pedant 1.0.29
* Add tests for superuser password authentication

### opscode-account rel-1.49.0
* Prevent password authentication for pivotal superuser

### opscode-platform-debug rel-0.4.6
* Remove legacy chargify code
* Updated knifetests to work with the latest reporting API

### private-chef-cookbooks
* platform_family fixes to couchdb and drbd cookbooks
* Set random initial password for pivotal user on bootstrap

## 11.1.3 (2014-04-09)

### berkshelf
* new dep: libffi
* new dep: libarchive

### curl 7.36.0
* CVE-2014-0138: libcurl can in some circumstances re-use the wrong connection when asked to do transfers using other protocols than HTTP and FTP
* CVE-2014-0139: libcurl incorrectly validates wildcard SSL certificates containing literal IP addresses when built to use OpenSSL
* CVE-2014-1263: When asked to do a TLS connection (HTTPS, FTPS, IMAPS, etc) to a URL specified with an IP address instead of a name, libcurl built to use Darwinssl would wrongly not verify the server's name in the certificate
* CVE-2014-2522: When asked to do a TLS connection (HTTPS, FTPS, IMAPS, etc) to a URL specified with an IP address instead of a name, libcurl built to use Winssl would wrongly not verify the server's name in the certificate

### chef
* upgrade to version 11.10.4

### erlang
* upgrade to r15b03-1

### nokigiri
* upgrade to nokigiri 1.6.1

### libyaml 0.1.6
* CVE-2014-2525: Heap-based buffer overflow allows context-dependent attackers to execute arbitrary code

### oc_erchef 0.24.2
* add oc_chef_action to oc_erchef (support for opscode-analytics actions package)

### openssl 1.0.1g
* CVE-2014-0160: heartbeat extension allows remote attackers to obtain sensitive information from process memory

### opscode-account 1.48.0
* fix USAG and organization creation for sql
* fix bug where billing-admins creation crashed for sql
* gracefully fail association request if org is in 504 mode
* speed up internal org-creation by removing Couchdb _all_dbs call
* check org _route endpoint for groups darklaunch during org creation
* fix schema constraint bug during LDAP user creation

### opscode-webui 3.8.13
* Ruby on Rails security updates

### postgresql
* upgrade to 9.2.8

### private-chef-cookbooks
* Increase postgresql max_connections to 350 to handle 4 node cluster
* Manage permissions for /var/log/opscode for non 0022 umasks

### private-chef-ctl
* Remove incorrect mention of `heartbeat_device` from `ha-status` output.

### chef-pedant 1.0.27
* added CLI options for running /internal-organization endpoint tests
* added tag for running organization tests
* add association tests to tags list

### oc-chef-pedant 1.0.28
* added test coverage for /organization and /internal-organization endpoints
* added association framework and tests

## 11.1.2 (2014-02-28)

### posgresql
* Add ossp-uuid extension to Postgres 9.2

### libossp-uuid 1.6.3
* Add libossp-uuid library for Postgres

### private-chef-cookbooks
* Configure oc_actionlog in oc_erchef and rabbit
* Remove :session and :environment from webui exception emails
* Add internal /_routes endpoint to load balancer

## 11.1.1 (2014-02-17)

### private-chef-cookbooks

#### BUGFIXES
* remove banned/whitelist IP checking from OpenResty Lua config that breaks ipv6 clients

## 11.1.0 (2014-02-06)

### omnibus-ruby 1.3.0
* https://github.com/opscode/omnibus-ruby/blob/master/CHANGELOG.md#130-december-6-2013

### omnibus-software 3d9d097332199fdafc3237c0ec11fcd784c11b4d
* [keepalived] update to 1.2.9 + patch for Centos 5.5
* [perl] generate an Omnibus-friendly CPAN config
* [openssl] CVE-2013-4353/CHEF-4939 - tls handshake causes null pointer in OpenSSL
* [berkshelf] update to 2.0.12
* [libyaml] CVE-2013-6393 - update libyaml to 0.1.5

### redis-rb 3.0.6
* Add redis gem for reconfigure management of redis install

### openresty-lpeg 0.12
* Add Lua lpeg library for use in refactored openresty routing config

### redis 2.8.2
* Add back in for use in openresty routing config

### bookshelf 1.1.3
* Remove request logging, which causes backups and crashing under heavy load

### enterprise-chef-server-schema 2.2.3
* Add containers table
* Add new enum type and columns for user password hash
* Add groups table
* Add index for opc_users(customer_id) (improves delete performance)

### oc-chef-pedant 1.0.25
* [CHEF-4086] Add tests for cookbook version host header changes
* Add tests to validate newly created organizations
* Updates to /containers endpoint tests for ruby / erlang switching
* Updates to /groups endpoint tests for ruby / erlang switching
* Use IPV6-compatible rest-client gem for testing IPV6
* Add tests for /users/:user/_acl endpoint
* Update /principals endpoint tests for pushy updates

### oc_bifrost 1.4.4
* Add IPV6 support
* Use shared opscoderl_wm to pull in webmachine dependency

### oc_erchef 0.23.0
* [CHEF-4086] Add configurable host for S3 pre-signed URLs
* Refactor chef_objects, chef_db, and chef_wm to support non-open-source features
* Add support for SQL/Erlang /containers endpoint (not migrated)
* Add support for SQL/Erlang /groups endpoint (not migrated)
* Convert all configuration fetching code to use envy library
* Remove REST API for darklaunch
* Add containers API docs to oc_erchef code base
* Remove caching of search-related database responses
* Remove fast_log and replace with lager
* Add IPV6 support
* Differentiate between 404s for missing principal vs. missing org

### opscode-account rel-1.43.0
* Remove SQL switching code for migrated objects
* Support container objects in SQL
* Support group objects in SQL
* Remove obsolete clients controller
* Encrypt user passwords with bcrypt
* BUGFIX: allow non-admin users to leave organizations
* Remove UPDATE from containers API
* Add IPV6 support
* BUGFIX: fix Ace.new method in #update_user_ace
* BUGFIX: don't log password changes in plain text
* BUGFIX: /organizations API can't show billing admins group

### sqitch
* Ensure sqitch uses an Omnibus-specific CPAN config

### private-chef-cookbooks
* [keepalived] Adjust command syntax for 1.2.9
* [erchef / bookshelf] Add s3_external_url configuration
* [all] Add IPV6 address support
* [nginx] Add ipv6only option to listen directive
* [sysctl] Force net.ipv6.bindonly to 0
* [opscode-certificate] Run certificate service on front-ends
* [redis] Add redis back into EC build (name redis-lb)
* [enterprise-chef-server-schema] Add schema upgrade for bcrypt user password support
* [openresty] Add lua-based upstream routing
* [oc_bifrost] Use opscoderl_wm logging
* [oc_erchef] Replace fast_log with lager
* [oc_erchef] Remove deprecated use of db_type for sqerl config
* [configuration] Increment api_version for release 11.0.0 -> 11.1.0
* [opscode-certificate] Make sure :restart action occurs on all nodes
* [keepalived] Fixes for keepalived.conf to work with 1.2.9 unicast
* [bookshelf] Turn off request logging
