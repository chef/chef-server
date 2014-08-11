# Enterprise Chef Release Notes

## 12.0.0 (Unreleased)

### What's New:

The following items are new for Enterprise Chef 11.1.5 and/or are changes from previous versions:

* [opscode-webui] Opscode WebUI has been removed in favor of the Manage Console add-on.
* [private-chef-cookbooks] Introduce pluggable HA architecture as alternative to DRBD.
* [private-chef-cookbooks] Add bifrost_sql_database uri to orgmapper.conf
* [opscode-platform-debug] Upgrade to rel-0.5.1
* [private-chef-ctl] Add a gather-logs command to create a tarball of
  important logs and system information.

### Bug Fixes:

The following items are the set of bug fixes that have been applied since Enterprise Chef 11.1.5:

* [opscode-omnibus] private-chef-ctl test command should provide pedant return code.
* [opscode-omnibus] Use more strict regular expression for IP check in ha-status
* [opscode-omnibus] Ensure CouchDB compaction cron job does not run on passive backend.
* [OC-11575][enterprise-chef-common] Don't start services by default in HA topology
* [OC-11601] - Correct another case where redis_lb was not started before attempting to reconfigure it
* [OC-11672] Upgrade PostgreSQL to 9.2.9
* [OC-11710] Fix couchdb compaction log rotation
* [OC-11703] Fix bug that prevents ACL and group expansion when containing group that no longer exists
* [OC-11708] Fix user association bug when last updater of users group is no longer associated
* [OC-11297] tweak partybus migration-level subscribes for a more reliable workaround

### Security Fixes:

The following items are the set of security fixes that have been applied since Enterprise Chef 11.1.5:

* OpenSSL 1.0.1i addresses CVE-2014-3512, CVE-2014-3511, CVE-2014-3510, CVE-2014-3507, CVE-2014-3506, CVE-2014-3505, CVE-2014-3509, CVE-2014-5139, and CVE-2014-3508.

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
