# Chef Server Changelog

## 12.0.6 (undetermined)

### bookshelf 1.1.7
* Uses relx for Erlang application releases
* Upgraded to lager 2.1.1

### oc\_bifrost 1.4.6
* Uses relx for Erlang application releases
* Upgraded to lager 2.1.1

### oc\_erchef 1.6.2
* Uses relx for Erlang application releases
* Upgraded to lager 2.1.1

### oc-chef-pedant 2.0.1
* Adds tests for keys named get
* Integrates chef-pedant into oc-chef-pedant.

### oc\_erchef 1.6.1
* Integrates schema into oc\_erchef itself
* Adds policyfile validation support
* License and readme updates

## 12.0.5 (2014-02-26)

### bookshelf 1.1.6
* Updated to webmachine 1.10.8

### oc\_bifrost 1.4.5
* Updated to webmachine 1.10.8

### oc-chef-pedant 1.0.79
* New keys API tests
* New cookbook artifact API tests

### oc\_erchef 1.5.0
* Keys API POST support: /organizations/$ORG/clients/$CLIENT/keys and
  /users/$USER/keys
### oc\_erchef 1.5.0
* Keys API POST support: /organizations/$ORG/clients/$CLIENT/keys and
  /users/$USER/keys

### oc\_erchef 1.4.2
* the fields `external_authentication_uid` and `recovery_auth_enabled`
  are now preserved on user PUT when they are not provided.

### oc\_erchef 1.4.1
* New GET/POST `BASE_URL/cookbook_artifacts/NAME/IDENTIFIER` endpoint
* Updated to webmachine 1.10.8

## 12.0.4 (2014-02-19)

### opscode-omnibus
* nginx bookshelf caching, enabled with
  `opscode_erchef['nginx_bookshelf_caching'] = :on`
* s3 URL expiry window setting,
  `opscode_erchef['s3_url_expiry_window_size']`, which can have values
  in minutes (e.g. `"15m"`), percentage (e.g. `"15%"`), or just be
  `:off`.
* Ensure shell metacharacters in arguments to chef-server-ctl user-
  and org- commands are properly handled.
* Pull in chef-client 12.0.3.
* Update rabbitmq cookbook to be compatible with modern chef-client.
* Update pivotal and knife-ec-backup knife configs to be compatible with modern chef-client.
* Use chef-client -z instead of chef-solo in the server.

### oc\_erchef 1.4.0
* keys API: new GET support for `/users/$user/keys` and `/organizations/$org/clients/$client/keys`
* module epgsql brought up to current.
* Fix LDAP regressions related to multiple fields, anonymous bind, and group\_dn

### oc\_erchef 1.3.1
* Add incubation feature for policyfiles. Feature flag off by default.

### oc\_erchef 1.2.2
* Add `s3_url_expiry_window_size` setting for s3 URL caching.

### oc-chef-pedant 1.0.76
* test support for keys API endpoint (GET)

### oc-chef-pedant 1.0.75
* test support for policyfile endpoints

### omnibus-ctl 0.3.2
* Use chef-client -z instead of chef-solo.
* Reference chef-client via `base_path`.

### knife-ec-backup 2.0.1
* Added keys table / key rotation support.

### ruby 2.1.4
* Needed for ohai >= 2.

### chef-gem 12.0.3
* [chef-client 12 changelog](https://docs.chef.io/release_notes.html#what-s-new).

## 12.0.3 (2015-02-04)

### enterprise-chef-common 0.5.1
* Add preliminary systemd support

### enterprise-chef-common 0.5.0
* Make it possible to pass arbitrary attrs to runit resources

### chef-pedant and oc-chef-pedant
* Updated chef-pedant to 1.0.41, oc-chef-pedant to 1.0.73. These
  versions have been updated to use RSpec 3.

### opscode-omnibus
* Added key management and rotation commands add-client-key,
  add-user-key, delete-user-key, delete-client-key, list-client-keys,
  and list-user-keys.
* Pulled in Chef 11.18.0. This will fix "ffi-yajl and yajl-ruby gems
  have incompatible C libyajl libs" warning when running
  chef-server-ctl commands.
* Ensure nginx restarts on frontends after lua-related changes
* Updated nginx's logrotate config with proper log ownership.
* Nginx logs $http_x_forwarded_for instead of $remote_addr if
  nginx['log_x_forwarded_for'] is true. The default is false
* Log an error and exit when DRBD mount attempts are
  exhausted rather than entering an infinite loop.
* Fix installation errors caused by PERL5LIB environment
  variable
* chef-server-ctl now returns non-zero exit codes for errors
  during user and organization-related commands.
* Use -D for --download-only option in
  chef12-upgrade-download command, avoiding option name conflict.


### oc\_erchef 1.2.0
* add basic multikey/key rotation support. This is not yet exposed via
  the REST API, but is being used within `oc_erchef` itself.

### oc\_erchef 1.1.1
* Updated `sqerl` version to pull in more current `epgsql` dependency
* Pulled repos `chef_db`, `chef_index`, `chef_objects`, `depsolver`,
  `oc_chef_authz`, and `oc_chef_wm` into apps in `oc_erchef`.
* Pulled `chef_wm` into `oc_chef_wm`.
* Updated integration tests, and got integration and unit tests
  running in Travis CI.
* Remove array merging in `chef_deep_merge`, fixing incorrect search
  results for arrays.

### opscode-chef-mover 2.2.19
* Updated mover to pull in oc\_erchef since some dependencies where moved there.

### enterprise-chef-server-schema 2.4.1
* Use HTTPS instead of GIT to pull down dependencies in Makefile.

### opscode-omnibus
* merged `oc_erchef` configuration sections for `chef_wm` into `oc_chef_wm`

## 12.0.2 (2015-01-27)

### chef-mover 2.2.20
* Fix bug that can cause long-running migrations to hang indefinitely

### private-chef-cookbooks
* Expose configurable value for database bulk fetch batch size to
  use during Solr 4 migrations

## 12.0.1 (2014-12-17)

### oc-id
* Update to version 0.4.4 to patch a doorkeeper CSRF vulnerability

### chef-mover
* update to version 2.2.17, with better failure case handling and
  increased timeouts.

### oc-chef-pedant 1.0.68
* pin mixlib-shellout to 1.6.1

### opscode-omnibus
* pin mixlib-shellout to 1.6.1
* added new `group_dn` ldap attribute to require users to be in the
  named group.
* Refactored superuser bootstrap process to use new chef-server-bootstrap
  repository instead of opscode-test, which pulled in a variety of now
  deprecated ruby repositories.
* Update location/name of Chef’s public GPG key.
* Fetch chef-server-ctl man page directly from chef-docs repo.

### chef-server-bootstrap 1.0.0
* Repository that replaces opscode-test, allowing us to deprecate several
  old ruby repositories.

### oc\_erchef 0.30.0
* module `chef_wm` merged into `oc_chef_wm`
* support for ldap user search including memberOf group,
  via attribute `group_dn`

## 12.0.0 (2014-11-25)

### enterprise-chef-common 0.4.7
* Restart logging service on log configuration change

### enterprise-chef-common 0.4.6
* Make project-ctl configurable by name

### omnibus-ctl 0.3.1
* Exclude gz files from tail

### private-chef-cookbooks
* Add `ip_mode` and `normalize_host` for ipv6 configuration
* Add configuration for queueing in pooler
* Expose `db_timeout` for sqerl in Erchef, bifrost and mover as a parameter
  that can be set in the "/etc/opscode/chef-server.rb" file for convenience.
  By default there is a hard coded value of 5 seconds (5000ms) as per:
  [sqerl\_client.erl](https://github.com/opscode/sqerl/blob/master/src/sqerl_client.erl#L134)
* Select appropriate default port for LDAP and LDAPS (when encryption is
  selected, as previously user had to manually add port to make it work).
* Expose `proxy_connect_timeout` for Nginx when it connects to the backends,
  so it can be adjused. The hard coded default might not be sufficient in
  some cases.
* Expose `folsom_graphite` configuration, default to disable
* Move Postgres database stop/start out of migrations
* Gracefullly attempt to start the database during migrations

### opscode-omnibus
* Add ability to configure SQL query timeout for Erchef, bifrost and mover.
* Provide reasonable default for LDAP and LDAPS ports.
* Deprecate ldap "encryption" setting and replace with
  `ssl_enabled`/`tls_enabled`. Add further validation and sanity checks around
  ldap settings, as well as deprecation warnings.
* Add ability to configure timeout for connect() when connecting to backends.

### oc\_erchef 0.29.4
* fix issue in which local mode auth was not handled correctly,
  preventing accounts on an LDAP server from being associated
  with existing Chef Server accounts when the login name differed.

### oc-chef-pedant 1.0.67
* Modify test of local mode authentication to be correct

### oc-chef-pedant 1.0.66
* Turn org creation validation off by default

## 12.0.0.rc6 (2014-11-11)

### oc-chef-pedant 1.0.65
* Add test for /organizations/:org\_id/ANY/\_acl endpoint

### oc-chef-pedant 1.0.64
* Add coverage for /users/USER/organizations endpoint

### oc-chef-pedant 1.0.63
* additional test for proper behavior when attempting to remove an org's
  admin.
* Update tests to reflect that clients no longer have C/U/D permissions
  on data bags by default.

### oc-chef-pedant 1.0.62
* Fix for consistent return values in oc\_erchef

### oc\_erchef 0.29.3
* route /organizations/:org\_id/ANY/\_acl endpoint

### oc\_erchef 0.29.2
* set default client ACLs for data bags to read-only.  See Release Notes for i
  important related details.
* correct message logging in org-user association/disassociation process
* new /controls endpoint in support of upcoming client features

### oc\_erchef 0.29.1
* revert functionality change where erchef version of /users/X/organizations
  endpoint no longer returned "guid" field. This field is used by internal
  products  in our hosted environment and cannot yet be removed.
* fix regression in which organization user was partially removed
  even though removal was disallowed because user is an admin.
* update actions to support capture of acl activity

### oc\_erchef 0.29.0
* Internal placeholder we used to indicate our *hosted* product
  switch from Erlang R15B03-1 to R16B03-1.  Note that R16B03-1 has been
  included in CS12 since the first RC.

### oc\_erchef 0.28.5
* update sqerl to use queuing-enabled pooler API
* update pooler to 1.3.3, which adds queueing support

### oc\_erchef 0.28.4
* Add folsom-graphite dependency (used for runtime stats gathering)

### oc\_erchef 0.28.3
* fix regression that broke org caching
* Org support in postgres
* Reindexing support to check redis flags
* Fix typo in darklaunch interrogation

### oc\_id
* Set `VERSION` environment variable on database migrations to avoid conflict
  during upgrades

### opscode-omnibus
* changes to addon installs to default to lucid when current ubuntu codename isn't in the accepted list (to support installs on 14)
* added apt-transport-https package in case it was missing from the system (packagecloud requires it)
* created chef-server.rb during install to cut down on user confusion
* [opscode-omnibus-597] Limit postgresql shared memory usage to stay under SHMAX
* Change postgres effective\_cache\_size to 50% of available RAM instead of hard coding at 128MB
* updated references to omnibus-ruby repo to be omnibus
* changelog - fix markdown formatting errors
* changelog - added this changelog note

### private-chef-cookbooks
* [OC-11769] make oc\_chef\_authz a tunable in private-chef.rb
* Fix oc\_chef\_authz timeout tunable
* Make postgresql slow query logging configurable
* Fix missing resources on API HTML pages
* Fixed the default value for Postgres effective\_cache\_size
* Adjust perms to 0750 for all service's log dir
* Add and use new perms attribute
* Add an OmnibusHelper method to provide an owner and group hash

### chef-server-ctl
* Partition server start/stop in upgrade process
* Changed commands org-associate and org-dissociate to org-user-add and org-user-remove, respectively.
* Update password command to use knife-opc so as to work post-removal of mixlib-authorization.

## 12.0.0.rc5 (2014-10-17)

### openssl - 1.0.1j
- SRTP Memory Leak (CVE-2014-3513)
- Session Ticket Memory Leak (CVE-2014-3567)
- Build option no-ssl3 is incomplete (CVE-2014-3568)

### opscode-omnibus
* properly configure ldap under erchef, and add some safeguards
  against incorrect encryption configuration.
* oc\_erchef updated to 0.27.4
* Bump the chef\_max\_version to 12 (this is the max chef client version that Chef Server will accept)
* expose license configuration options
* Add man page for chef-server-ctl.
* Correct gather-logs to point to chef-server.rb
* Disable SSLv3 support in nginx
* Added command line options to open-source-to-chef-server-12 upgrade for finer-grained control of migration process

### oc\_erchef 0.27.7
* Improve error handling in org creation and deletion.

### oc\_erchef 0.27.6
* Fixed pooler bug with regard to timed out pool member starts

### oc\_erchef 0.27.5
* Add org info to actions

### oc\_erchef 0.27.4
* ldap start\_tls support
* ldap simple\_tls support
* support for correctly looking up users by external auth id
* fix for GET of org users not returning correct state record, resulting
  in requests not properly terminating

### oc\_erchef 0.27.3
* Fix meck dependency locking issue.

### oc\_id 0.4.2
* Add support for Chef signed headers in Resource Owner Password
  Credentials flow
* Add new endpoint (/v1/me/organizations) to get the list of
  organizations for the user represented by a Bearer token
* Update doorkeeper gem to 1.4.0
* Add support for Resource Owner Password Credentials flow

### opscode-chef-mover 2.2.15
* Clean up error handling for org user associations and invites migrations
* Fix backwards compatibility issues with oc\_chef\_authz intergration

### rest server API
* removed check for maximum client version (only checks for minimum, i.e., <10)
* updated server flavor from 'ec' to 'cs' (Chef Server) now that servers have been merged

### chef-server-ctl
* Restricted chef-server-ctl install to known Chef packages
* Correct show-config command/recipe to point at chef-server.rb instead of private-chef.rb
* Updated knife-opc config so that user / org / association commands now work if non-default ports are used.
* re-enable ctrl+c for chef-server-ctl commands by setting "client\_fork false" in solo.rb

### omnibus-ctl 0.3.0

* Extended API with `add_command_under_category`, that allows ctl projects to group commands under categories, resulting in more logical help output.
* Added concept of hidden services that hides certain services from those listed in `chef-server-ctl status`.
* Any service (even hidden ones) can still be status checked via `chef-server-ctl status <service>`.
* opscode-chef-mover was added as a hidden service.

### oc-chef-pedant 1.0.60
* add support for ssl version configuration

### oc-chef-pedant 1.0.59
* Fix rspec deprecations
* Remove test of curl

## 12.0.0.rc4 (2014-09-17)

### opscode-omnibus
* Ensure contents of install dir (`/opt/opscode`) are owned by root.
* Configure oc-chef-pedant ssl version to match nginx

## 12.0.0

### Renamed chef server core instead of Private Chef or Enterprise Chef.

### opscode-omnibus
* Change to using /etc/opscode/chef-server.rb from /etc/opscode/private-chef.rb
* Symlink private-chef.rb to chef-server.rb if private-chef.rb is present

### bookshelf 1.1.4
* Erlang R16 support

### cacerts 2014/08/20
* Update to latest cacerts as of 2014/08/20

### chef-ha-plugin
* Add support for pluggable high availability system

### chef-sql-schema removed
* We use a sqitch based schema instead.

### couchdb removed
* We are pleased to announce that we have migrated all data over to sql.

### enterprise-chef-server-schema 2.4.0
* Updates org\_migration\_state table with migration\_type and verification
* Update org\_migration\_state with support for solr 4 migration
* Cleans up reporting schema info table
* Clean up Makefile to preserve PATH variable
* Update password hash type for OSC password hash types
* Fix constraints for org\_user\_assocations and org\_user\_invites
* Add tables for organizations, org\_user\_associations, and org\_user\_invites

### erlang R16B03 added
* Replaced R15, which was only used by the services we removed.

### knife-ec-backup
* Add support for tools to backup and restore from chef servers.

### oc-chef-pedant 1.0.57
* Remove /system-recovery endpoint tests
* Enhance test coverage for user-org association
* Update acl, organization and association tests for ruby-erlang differences
* Add tests for
  * authenticate\_user endpoint
  * users email validation
  * superuser access
  * certs in pubkey field for user
  * default organization rewriting
  * verify-password

### oc\_authz\_migrator removed
* oc\_authz\_migrator is no longer needed

### oc\_erchef updated to 0.27.3

#### oc\_erchef 0.27.3
* Organizations in erchef and in sql
* organization association and invites in erchef and sql

#### oc\_erchef 0.26
* Initial low level work for organizations and associations in SQL
* Improve reindexing script
* ACL endpoint in erchef
* Add chef action data\_payloads

#### oc\_erchef 0.25
* Add default organization support for OSC compatibility
* Add license endpoint support
* Add global placeholder org macro.
* System recovery endpoint work: Fix so recovery\_authentication\_enabled is correct for new users
* Add internal chef keygen cache to replace opscode-certificate service.
* do not force user key type to public on regeneration
* Bugfix for concurrent cookbook uploads
* Automatically upgrade user password salt algorithm on auth
* Cleanups for user password encryption
* Groups endpoing in sql and in erchef
* Update authenticate\_endpoint for LDAP
* Update chef users email validation and filtering
* Add chef users endpoint.

### opscode-account removed
* The last remaining endpoints (organizations, and user-org
  association and invites) are entirely implemented in erchef now.

### opscode-certificate removed
* This is replaced by the keygen service in erchef.

### opscode-chef-mover 2.2.14
* Organizations, user-org association, and user-org invite migrations from couchdb to SQL
* Migration of global containers and global groups from couchdb to SQL
* Backwards incompatible API change: Group creation (POST) ignores users and clients
* Containers and groups migration from couchDB to postgreSQL
* Bcrypt user migrations
* Solr4 migration
* Generalized migrate scripts and other code to be migration\_type agnostic
* Improved support for non-org based migrations
* Update for Erlang R16

### opscode-org-creator removed
* Erchef no longer needs multi-phase organization create; direct creation is sufficient.

### opscode-platform-debug and orgmapper removed
* Orgmapper is no longer useful after migrations to SQL are complete.

### Replace solr 1.4 with solr 4
* Upgrade to solr 4.

### Remove opscode-webui.
* It is superceded by the opcsode-manage package

### postgresql 9.1 removed

### private-chef-administration
* Removed. Docs can be found at docs.opscode.com

### private-chef-cookbooks
* Introduce pluggable HA architecture as an alternative to DRBD
* [OC-10117] opscode-solr4 accepts Java-like memory attributes
* [OC-11669] keepalived safe mode

### ruby updated to 1.9.3-p547
* Update is from 1.9.3-p484

### unicorn removed
* No longer needed because opscode-account is gone

### chef-server-ctl
* Renamed from private-chef-ctl
* Added chef-server-ctl upgrade command to support migrations from the open source chef 11 server
* Added tooling to manage users and orgs from the command line via knife-opc
* Added chef-server-ctl install command to install chef add-on packages (via web or local file)
* Clarify the use of the --path options for the `install` subcommand

### omnibus-ctl
* [OC-10470] Allow private-chef-ctl status to ignore disabled services.
* [OC-11574] private-chef-ctl service commands should be HA aware
* [OC-9877] exclude binary files and archives from \*-ctl tail

## 11.2.2 (2014-09-17)

### opscode-omnibus
* Ensure contents of install dir (`/opt/opscode`) are owned by root.

## 11.2.1 (2014-08-29)
### enterprise-chef-common
* Update to 0.4.5
* Fix issue where 'private-chef' was being changed to 'private\_chef' unexectedly in upstart/runit files

## 11.2.0 (2014-08-29)

### Makefile
* Add Makefile for automating builds

### adding actions\_payload 2014.08.15
* [CA-555] Update 11.1-stable oc\_erchef with latest oc\_chef\_action

### postgresql 2014.07.29
* [OC-11672] Upgrade PostgreSQL to 9.2.9

### enterprise-chef-common 2014.07.21
* [OC-11575] Don't start services by default in HA topology
* Update to 0.4.4

### oc\_chef\_actions 2014.07.03
* Update to latest of oc\_chef\_action to get hostname from fqdn instead
  of inet
* Setting the CHEF\_ACTIONS\_MESSAGE\_VERSION to 0.1.0
* Sets ['dark\_launch']['actions'] = true

### cacerts 2014.04.22
* Update to latest cacerts as of 2014-04-22

### chef 11.12.2
* Update embedded chef gem to 11.12.2

### opscode-platform-debug rel-0.5.1
* Add authz API support

### opscode-software
* Refactor PERL Postgres driver installation

### private-chef-cookbooks
* [analytics] Copy webui\_priv into opscode-analytics if actions is enabled
* [OC-11297] Tweak partybus migration-level subscribes for a more reliable
  workaround
* [OC-11459] Allow opscode-manage to easily be moved off of 443
* [OC-11540] Fix invalid opscode-account config when forcing SSL
* [OC-11601] Fix a race condition that sometimes caused redis\_lb to attempt to
  reconfigure itself before it was restarted.
* [OC-11668] Enable ipv6 in standalone mode
* [OC-11673] Tune PostgreSQL keepalive timeouts
* [OC-11710] Fix couchdb compaction log rotation
* Add bifrost\_sql\_database uri to orgmapper.conf
* [OC-11585] Allow ['lb']['upstream'] to have a custom setting
* [CHEF-3045] increase s3\_url\_ttl from 15m to 8h
* Use SSL port for lb\_internal if non-SSL is disabled
* Lock down postgresql

### private-chef-ctl

* Add a gather-logs command to create a tarball of important logs and
  system information for Chef Support
* [OC-9877] Fix bug that included binary files and archives when using
  'private-chef-ctl tail'

### oc-id 0.3.3
* Add Chef Identity Service (oc-id)

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

### rabbitmq 3.3.4
* Upgrade to RabbitMQ 3.3.4

### opscode-account rel-1.51.0
* [OC-11702] - fails to expand ACLs and groups when they contain
  groups that no longer exist
* [OC-11708] - fixes user association bug that relied on permissions
  of the last updater of the users group

## 11.1.8 (2014-06-26)

### oc\_authz\_migrator 0.0.2
* exit immediately on errors


## 11.1.7

### private-chef-cookbooks
* [OC-11499] Use more strict regular expression for IP check in ha-status
* [OC-3107] Ensure CouchDB compaction cron job does not run on passive
  backend.
* [OC-11601] Restart redis\_lb immediately during reconfigure
* [OC-11490] Explicitly set keepalived directory ownership
* [OC-11297] EC 11 fresh install not saving migration state
* [OC-11656] Set explicit owner and group for services without them
* Address a PostgreSQL configuration error. The defect allows any local user on the system hosting the Chef Server’s PostgreSQL components full access to databases.
* [OC-11662] Separate redis\_keepalive\_timeout from redis\_connection\_timeout and increase their default values from 60ms to 1000 and 2000ms, respectively.

### private-chef-ctl
* [OC-11657] Bump default svwait timeout of 7 seconds to 30 seconds
* [OC-11382] keepalived restart interferes with upgrades
* [OC-8881] private-chef-ctl password does not work

### configurable postgresql unix user
* Update gather-logs and migration scripts to honor postsgresql['username']

## 11.1.6 (2014-06-05)

### openssl 1.0.1h
* Address vulnerabilities CVE-2014-0224, CVE-2014-0221, CVE-2014-0195,
  CVE-2014-3470 https://www.openssl.org/news/secadv\_20140605.txt
  return code

### private-chef-cookbooks
* [OC-11581] private-chef-ctl test command should return the pedant
  return code

## 11.1.5 (2014-05-14)

### oc\_erchef 0.24.6
* rename oc\_actionlog to actions

### private-chef-cookbooks
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
* platform\_family fixes to couchdb and drbd cookbooks
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

### oc\_erchef 0.24.2
* add oc\_chef\_action to oc_erchef (support for opscode-analytics actions package)

### openssl 1.0.1g
* CVE-2014-0160: heartbeat extension allows remote attackers to obtain sensitive information from process memory

### opscode-account 1.48.0
* fix USAG and organization creation for sql
* fix bug where billing-admins creation crashed for sql
* gracefully fail association request if org is in 504 mode
* speed up internal org-creation by removing Couchdb \_all\_dbs call
* check org \_route endpoint for groups darklaunch during org creation
* fix schema constraint bug during LDAP user creation

### opscode-webui 3.8.13
* Ruby on Rails security updates

### postgresql
* upgrade to 9.2.8

### private-chef-cookbooks
* Increase postgresql max\_connections to 350 to handle 4 node cluster
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
* Configure oc\_actionlog in oc\_erchef and rabbit
* Remove :session and :environment from webui exception emails
* Add internal /\_routes endpoint to load balancer

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
* Add index for opc\_users(customer\_id) (improves delete performance)

### oc-chef-pedant 1.0.25
* [CHEF-4086] Add tests for cookbook version host header changes
* Add tests to validate newly created organizations
* Updates to /containers endpoint tests for ruby / erlang switching
* Updates to /groups endpoint tests for ruby / erlang switching
* Use IPV6-compatible rest-client gem for testing IPV6
* Add tests for /users/:user/\_acl endpoint
* Update /principals endpoint tests for pushy updates

### oc\_bifrost 1.4.4
* Add IPV6 support
* Use shared opscoderl\_wm to pull in webmachine dependency

### oc\_erchef 0.23.0
* [CHEF-4086] Add configurable host for S3 pre-signed URLs
* Refactor chef\_objects, chef\_db, and chef\_wm to support non-open-source features
* Add support for SQL/Erlang /containers endpoint (not migrated)
* Add support for SQL/Erlang /groups endpoint (not migrated)
* Convert all configuration fetching code to use envy library
* Remove REST API for darklaunch
* Add containers API docs to oc\_erchef code base
* Remove caching of search-related database responses
* Remove fast\_log and replace with lager
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
* BUGFIX: fix Ace.new method in #update\_user\_ace
* BUGFIX: don't log password changes in plain text
* BUGFIX: /organizations API can't show billing admins group

### sqitch
* Ensure sqitch uses an Omnibus-specific CPAN config

### private-chef-cookbooks
* [keepalived] Adjust command syntax for 1.2.9
* [erchef / bookshelf] Add s3\_external\_url configuration
* [all] Add IPV6 address support
* [nginx] Add ipv6only option to listen directive
* [sysctl] Force net.ipv6.bindonly to 0
* [opscode-certificate] Run certificate service on front-ends
* [redis] Add redis back into EC build (name redis-lb)
* [enterprise-chef-server-schema] Add schema upgrade for bcrypt user password support
* [openresty] Add lua-based upstream routing
* [oc\_bifrost] Use opscoderl\_wm logging
* [oc\_erchef] Replace fast\_log with lager
* [oc\_erchef] Remove deprecated use of db\_type for sqerl config
* [configuration] Increment api\_version for release 11.0.0 -> 11.1.0
* [opscode-certificate] Make sure :restart action occurs on all nodes
* [keepalived] Fixes for keepalived.conf to work with 1.2.9 unicast
* [bookshelf] Turn off request logging
