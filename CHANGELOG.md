# Enterprise Chef Changelog

## 11.1.2

### oc_erchef
* oc\_erchef 0.24.0 - add oc\_chef\_action to oc\_erchef

### posgresql
* Add ossp-uuid extension to Postgres 9.2

### libossp-uuid 1.6.3
* Add libossp-uuid library for Postgres

### private-chef-cookbooks
* Configure oc_actionlog in oc_erchef and rabbit
* Remove :session and :environment from webui exception emails

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
