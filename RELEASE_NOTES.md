# Enterprise Chef Release Notes

## 12.0.0.rc4 (2014-09-17)

### Security Fixes
* [opscode-omnibus] Ensure contents of install dir (/opt/opscode) are owned by root.

## 12.0.0

### What's New:

The following items are new since Enterprise Chef 11.2.1 and/or are changes from previous versions:

* [opscode-omnibus] The chef_max_version has been bumped to 12, so that Chef clients up to 12 can connect to the server.
* [couchdb] has been removed
* [oc_erchef]
  * All endpoints that formerly were in opscode-account are now in erchef and the data
    resides in SQL. This includes containers, groups, organizations, org associations and invites.
    * The API around group creation (POST) now ignores users and clients
  * Key generation is now in erchef.
  * Server flavor returned from REST calls is now "sc"

* [opscode-account] has been removed
* [opscode-certificate] has been removed
* [opscode-org-creator] has been removed
* [orgmapper] has been removed
* [opscode-webui] Opscode WebUI has been removed in favor of the Manage Console add-on.
* [private-chef-cookbooks] Introduce pluggable HA architecture as alternative to DRBD.
* [private-chef-cookbooks] Add bifrost_sql_database uri to orgmapper.conf
* [private-chef-ctl] Add a gather-logs command to create a tarball of
  important logs and system information.
* [solr] has been upgraded to Solr 4


### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.2.1:

* [OC-11703] Fix bug that prevents ACL and group expansion when containing group that no longer exists
* [OC-10470] Allow private-chef-ctl status to ignore disabled services
* [OC-11574] private-chef-ctl service commands should be HA-aware
* [OC-9877] Exclude binary files and archives from 'omnibus-ctl tail'

## 11.2.1

### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.2.0:

* Fix issue where 'private-chef' was being changed to 'private_chef' unexectedly in upstart/runit files

## 11.2.0 (2014-08-29)

### What's New

The following items are new for Enterprise Chef 11.2.0 and/or are
changes from previous versions:

* [private-chef-cookbooks] Add bifrost_sql_database uri to orgmapper.conf
* [opscode-platform-debug] Upgrade to rel-0.5.1
* [private-chef-ctl] Add a gather-logs command to create a tarball of
  important logs and system information.
* [oc-id] Add Chef Identity Service.  This enables Supermaket authentication
  with the Chef Server.
* [opscode-analytics]
  * `dark_launch['actions']` defaults to true.  You no longer
  need to manually set this in the private-chef.rb
  * Copy webui_priv into opscode-analytics if actions is enabled
  * This change adds a new 'oc-id' key to the private-chef-secrets.json.
* [orgmapper] Bump orgmapper to a new minor revision.  This enables support for
  the bifrost/authz API and fixes several bugs.


### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.1.8:

* [OC-11297] tweak partybus migration-level subscribes for a more reliable workaround
* [OC-11585] Allow ['lb']['upstream'] to have a custom setting
* [OC-11459] Allow opscode-manage to easily be moved off of 443
* [OC-11540] Fix invalid opscode-account config when forcing SSL
* [OC-11575] Don't start services by default in HA topology
* [OC-11601] Fix a race condition that sometimes
  caused redis_lb to attempt to reconfigure itself before it was restarted.
  * This causes redis_lb to restart during every reconfigure.  This restart can
    cause a short period of 500 errors on the on the FE nodes.
* [OC-11668] enable ipv6 in standalone mode
* [OC-11672] Upgrade PostgreSQL to 9.2.9
* [OC-11673] Tune PostgreSQL keepalive timeouts
* [OC-11702] Fix bug that prevents ACL and group expansion when containing group that no longer exists
* [OC-11708] Fix user association bug when last updater of users group is no longer associated
* [OC-11710] Fix couchdb compaction log rotation

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 11.1.8:

* OpenSSL 1.0.1i addresses CVE-2014-3512, CVE-2014-3511, CVE-2014-3510, CVE-2014-3507, CVE-2014-3506, CVE-2014-3

## 11.1.8 (2014-06-26)

### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.1.6:

* [opscode-omnibus] private-chef-ctl test command should provide pedant return code.
* [opscode-omnibus] Use more strict regular expression for IP check in ha-status
* [opscode-omnibus] Ensure CouchDB compaction cron job does not run on passive backend.
* [OC-11499] Use more strict regular expression for IP check in ha-status
* [OC-3107] Ensure CouchDB compaction cron job does not run on passive backend.
* [OC-11601] Restart redis_lb immediately during reconfigure
* [OC-11490] Explicitly set keepalived directory ownership
* [OC-11297] EC 11 fresh install not saving migration state in HA topology
* [OC-11656] Set explicit owner and group for services without them
* [OC-11657] Bump default svwait timeout of 7 seconds to 30 seconds
* [OC-11382] keepalived restart interferes with upgrades
* [OC-8881] private-chef-ctl password does not work

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 11.1.6:

* Address a PostgreSQL configuration error. The defect allows any local user on the system hosting the Chef Serv






## 11.1.6 (2014-06-05)

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 11.1.5:

* Address vulnerabilities CVE-2014-0224, CVE-2014-0221, CVE-2014-0195, CVE-2014-3470 https://www.openssl.org/news/secadv_20140605.txt

## 11.1.5 (2014-05-14)

### What's New:
* [oc_erchef] First release with full compatibility for Chef Actions service

## 11.1.4 (2014-05-07)

### Security Fixes:

The following items are the set of security fixes that have been
applied since Enterprise Chef 11.1.3:

* [bootstrap] Set random initial password for pivotal superuser on bootstrap
* [opscode-account] Prevent password authentication for pivotal superuser

## 11.1.3 (2014-04-09)

### What's New:

The following items are new for Enterprise Chef 11.1.3 and/or are changes from previous versions:

* [core] Erlang r15b03-01 w/ multiple stability and bug fixes
* [core] Chef 11.10.4 (was 11.6.0)
* [core] PostgreSQL 9.2.8 (was 9.2.4)
* [oc_erchef] Added hooks for opscode-analytics actions service

### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.1.2:

* [opscode-omnibus] Increased postgresql max_connections to a default of 350 to handle 4 node clusters.
* [opscode-account] Fix for LDAP user creation failure.
* [opscode-omnibus] Manage /var/log/opscode permissions even with non 0022 umask.

* [opscode-omnibus] Separate redis_keepalive_timeout from redis_connection_timeout and increase their
  default values from 60ms to 1000 and 2000ms, respectively.

### Security Fixes:

The following items are the set of security fixes that have been
applied since Enterprise Chef 11.1.2:

* [opscode-webui] Patch for Denial of Service Vulnerability in Action View when using render :text (CVE-2014-0082)
* [opscode-webui] Patch for Denial of Service Vulnerability in Action View (CVE-2013-6414)
* [opscode-webui] Patch for Reflective XSS Vulnerability in Ruby on Rails (CVE-2013-4491)
* [libcurl] Patch for wrong re-use of connections (CVE-2014-0138)
* [libcurl] Patch for address wildcard certificate validation (CVE-2014-0139)
* [libcurl] Patch for not verifying certs for TLS to IP address / Darwinssl (CVE-2014-1563)
* [libcurl] Patch for not verifying certs for TLS to IP address / Winssl (CVE-2014-2522)
* [openssl] Patch for heartbeat extension exposing process memory (CVE-2014-0160)
* [libyaml] Patch for arbitrary code execution vulnerability (CVE-2014-2525)

## 11.1.2 (2014-02-28)

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 11.1.1:

* [opscode-webui] Don't log or email the Rails session or environment from the exception handler. Doing so can cause user-submitted form values like passwords to be logged and emailed to administrators of the Enterprise Chef server when exceptions occur on the Management Console.
