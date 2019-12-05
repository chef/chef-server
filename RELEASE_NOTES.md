# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current major release and all patches.
For prior releases,
see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 13.1.13 (2019-11-25)

### General changes

A lot of work for this release has gone into improving our test infrastructure to test the release with various topologies and configurations. This leads to increased confidence and reduced risk of regressions with future releases of Chef Infra Server. We have also addressed some technical debt, cleaning up our codebase and enabling us to better serve our community with increased velocity.

We have been performing issue triage for Chef Infra Server for three months, and we have been hard at work cleaning up GitHub issues to highlight active issues. Recordings of Chef Infra Server triage sessions can be found [here](https://www.youtube.com/user/getchef). Part of keeping our issue backlog relevant is ensuring that active issues are prominently featured over inactive issues. As a part of this effort we will be more aggressively closing issues that are inactive or do not apply to supported features. If you find that an issue or PR that you are invested in has closed, please feel free to reopen it. All feature requests can be made using the [Aha! Portal](https://chef-software.ideas.aha.io/).

Simplifying the management of Chef’s product portfolio is something that we are very focused on currently. As a part of that effort, we are preparing to move from Solr to Elasticsearch to align with several other projects and products that currently leverage Elasticsearch. Migration to Elasticsearch will also improve performance and provide an up-to-date dependency. Chef Infra server still continues to use Solr4 for search for the purposes of this release, but Elasticsearch packages are now also included albeit still unused.

### Improvements/bug fixes

- `_status` endpoint reports healthy even if data_collector is down not causing unnecessary failovers.
- Data collector proxy-header X-Forwarded for is set as expected.
- `chef-server-ctl` is no longer installed in the user path only the appbundled version is installed in the user path
- Fixes issue with Chef Support Zendesk sign-ins when a first name is not set in Hosted Chef.
- Added support for running the Chef Infra Server on Red Hat Enterprise Linux 8.
- Improvements to `chef-server-ctl gather-logs`
    - Add AWS to known platforms
    - Add AWS Native Chef Server info
    - Add elasticsearch info
    - Switched compression from `bzip2` to `gzip`

### Deprecation notices

- SLES 11 is no longer supported per our [platform policy](https://docs.chef.io/platforms.html#platform-end-of-life-policy), as upstream support ended in March of this year.

### Updates

- Postgres 9.6.10 -> 9.6.15
- Chef Infra Client v15.3.14 -> v15.4.45
- OpenResty 1.13.6.2 -> 1.15.8.1
- Nokogiri 1.8.5 -> 1.10.4 to resolve the following CVE's
    - [CVE-2019-5477](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2019-5477)
- Rebar3 -> 3.12.0
- Updated erlang deps to be the latest
- Loofah 2.2.3 -> 2.3.1
- Erlang R18 -> 20.3.8.9
- Updated ChefInfraServer setup cookbooks for cookstyle fixes
- Ruby 2.5.5 -> 2.6.3
- Openssl 1.0.2s -> 1.0.2t to resolve the following CVE's
    - [CVE-2019-1563](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2019-1563) and [CVE-2019-1547](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2019-1547) 

## 13.0.11 (2019-06-19)

### Chef Server is now Chef Infra Server

Chef Server has a new name, but don’t worry, it’s the same Chef Server you’ve grown used to. You’ll notice new branding throughout the application and documentation but the command `chef-server-ctl` remains the same.

### Chef EULA

Chef Infra Server requires an EULA to be accepted by users before it can be installed. Users can accept the EULA in a variety of ways:

- `chef-server-ctl reconfigure --chef-license accept`
- `chef-server-ctl reconfigure --chef-license accept-no-persist`
- `CHEF_LICENSE="accept" chef-server-ctl reconfigure`
- `CHEF_LICENSE="accept-no-persist" chef-server-ctl reconfigure`

Finally, if users run `chef-server-ctl reconfigure` without any of these options, they will receive an interactive prompt asking for license acceptance. If the license is accepted, a marker file will be written to the filesystem unless `accept-no-persist` is specified. Once this marker file is persisted, users no longer need to set any of these flags.

See our [Frequently Asked Questions document](https://www.chef.io/bmc-faq/) for more information on the EULA and license acceptance.

### Deprecation notice

- [Deprecated PowerPC and s390x platforms](https://blog.chef.io/2018/11/01/end-of-life-announcement-for-chef-server-for-linux-on-ibm-z-and-linux-on-ibm-power-systems/)
- [Deprecated Keepalived/DRBD-based HA](https://blog.chef.io/2018/10/02/end-of-life-announcement-for-drbd-based-ha-support-in-chef-server/)
- Deprecated Ubuntu 14.04 support. (Ubuntu 14 was EoL’d at the end of April 2019)

### Updates and Improvements

- Updated OpenResty to 1.13.6.2
    - This fixes two CVEs: CVE-2018-9230 and CVE-2017-7529.
    - This version cannot be built on PowerPC and s390x because those platforms are not supported in mainline luajit.
- Updated Ruby version to 2.5.5
- Updated Chef Infra Client to 14.11.21
- Updated runit cookbook to 5.1.1
- Migrated unit tests from Travis to Buildkite. Reorganized them for improved speed, stability and portability.
- Added some Habitat packaging improvements with parameterized search_server.
- Erchef request size increased from 1,000,000 to 2,000,000 bytes to better support inspec scanning via the audit cookbook.
- Nginx error logs no longer log 404s. In the Chef API, 404s are typically not errors as they are often the expected response about an object that doesn't exist. The logs will continue to show 404s in the request logs.
- Profiles and data-collector upstreams now render correctly if their root_url is configured. If the data_collector token secret is not set, a 401 response code and an error message will be seen instead of 404.

## 12.19.31 (2019-03-07)

This release was triggered by the update to Habitat base plans. (https://blog.chef.io/2019/01/28/base-plans-refresh-is-coming-to-habitat-core-plans/)
Omnibus release was done to keep in sync with the Habitat release.

- `chef-server-ctl` leverages HAB_LISTEN_CTL envvar if available.


## 12.19.26 (2019-01-31)

This release contains some minor improvements and updates to include software:

- Added request id to nginx log to easily track the Chef request through the logs.
- Bundler pinned to 1.17 to avoid taking the 2.0 upgrade.
- Erlang updated to 18.3.4.9
    - Fixed two CVEs CVE-2017-1000385 and CVE-2016-10253. SSL headers got stricter which unfortunately broke LDAP. (Issue #1642)
    - Removed `et`, `debugger`, `gs`, and `observer` as they depend on `wx`, which is not available on all platforms.
- Ruby updated to 2.5.3.
- Chef Client updated to 14.5.
- Erchef and Bookshelf can optionally use mTLS protocol for their internal communications.
- Added configuration for pedant SSL-signed requests to include mTLS support.
- Habitat package improvements:
    - Increased `authn:keygen_timeout` amount for `oc_erchef` hab pkg.
    - Removed `do_end` function from `chef-server-ctl` hab plan.
    - Enhanced `chef-server-ctl` to function in more habitat environments.
    - `chef-server-ctl` commands pass relevant TLS options during bifrost API calls.
- Used standard ruby-cleanup definition, which shrinks install size by ~5% on disk.
- Removed unused couchdb configurables.

## 12.18.14 (2018-10-15)

- Postgresql updated to 9.6.10
- In a standalone install postgresql listens on all loopbacks, not
  just localhost. This 
- Enhance bifrost to use SSL/TLS
- Updates to ruby libraries (rubyzip)
- Updates to erlang libraries (sqerl, epgsql)
- Improvements to the habitized chef-server 
- Segment free cookbooks are implemented. (https://github.com/chef/chef-rfc/blob/master/rfc067-cookbook-segment-deprecation.md) This bumps the API version
- acl for cookbook artifacts
- Security headers for HTTP
- Optional disabling of welcome page
- Ruby updated to 2.5.1
- Chef Client updated to 14.3
- `chef-server-ctl` now has version subcommand
- Datestamps are now in UTC
- Fixie is now shipped with chef server
- Security issue with old doorkeeper fixed by upgrading. [CVE-2018-1000211](https://nvd.nist.gov/vuln/detail/CVE-2018-1000211)

## 12.17.33 (2018-02-22)
This release upgrades the version of Ruby to 2.4.3.

- Add FIPS support for PPC64 (big-endian).
- Fixed invalid search query issue with elasticsearch where forward slashes not getting escaped properly..

## 12.17.15 (2017-12-21)

- Fix a regression in IPv6 address handling
- Allow disabling of request logging
  - opscode-erchef['enable_request_logging'] defaults to true
  - oc_bifrost['enable_request_logging'] defaults to true
  - bookshelf['enable_request_logging'] defaults to false
    - bookshelf is a change from prior behavior as this logging is redundant with the existing nginx logs
- `chef-server-ctl reconfigure` fixes permissions on gems with an overly restrictive umask
- Make the display of the welcome page configurable
  - nginx['show_welcome_page'] defaults to true
- Inferring the current database migration level and necessary upgrades for `chef-server-ctl upgrade`
- Catch server_name resolution errors during `chef-server-ctl reconfigure` and continue
- Add XSS related HTTP headers when chef-manage is not installed (chef-manage adds these itself)
- knife-opc upgraded to 12c02a to support prompting for a user password on creation
- Do not create the default rabbitmq guest useR
- Add /_acl endpoint for cookbook artifacts

## 12.17.5 (2017-10-24)

This release fixes a regression.
  - Set nginx['use_implicit_hosts'] = false by default.
    This fixes a regression for users deploying on Digital Ocean and
    potentially other non-AWS cloud platforms where
    nginx['use_implicit_hosts'] = true results in incorrect nginx configuration.

## 12.17.3 (2017-10-19)

This release has security updates and enhancements.
 - Update the version of java to 8u144 to address CVE-2017-3526.
 - Add a `/_stats` endpoint to Chef Server that conforms to Chef RFC 93
   (https://github.com/chef/chef-rfc/blob/master/rfc093-server-stats-endpoint.md)
   Currently, it exposes statistics about connection pool usage inside Erchef,
   Postgresql, and the Erlang VM. Further statistics will be added as needed.

## 12.16.14 (2017-09-21)

This release upgrades the version of Ruby to 2.2.8 to address multiple
upstream security vulnerabilities:

https://www.ruby-lang.org/en/news/2017/09/14/ruby-2-2-8-released/


## 12.16.9 (2017-08-30)

This release is a minor release addressing two problems in the
PostgreSQL 9.6 upgrade process

- Ensure that the disk has the required disk space before starting the
  PostgreSQL upgrade.

- Make pg_upgrade timeout configurable for users with large databases.
  Also, increase the default timeout to 2 hours.

## 12.16.1 (2017-08-15)

* [Upgrade to PostgreSQL 9.6](https://github.com/chef/chef-server/pull/1310),
  Chef Server now uses the latest stable version of the 9.6 series (9.6.3). Upgrades of existing
  installations are done automatically, but creating backups is advised.

  The information below only applies if you have set a custom value for `checkpoint_segments`
  in your `/etc/opscode/chef-server.rb`. If you have not set a custom value, there is nothing to
  change:

  The `checkpoint_segments` configuration setting is gone, so if you before have set

      postgresql['checkpoint_segments'] = 10

  you now have to specify (for example, please consult the documentation linked below)

      postgresql['max_wal_size'] = '3G'

  The default setting for `max_wal_size` is 1G. [The PostgreSQL release notes](https://www.postgresql.org/docs/9.6/static/release-9-5.html) mention a conversion
  rule: `max_wal_size = (3 * checkpoint_segments) * 16MB`. They also say that the default value
  for `max_wal_size` (1GB) should fit in most settings, so this conversion is not done automatically.

  The `shmmax` and `shmall` configuration settings are no longer used as PostgreSQL 9.6 relies on
  System V shared memory much less than PostgreSQL 9.2. The `shared_buffers` configuration setting
  is still respected and can be used to modify the amount of shared memory used by PostgreSQL.

  This update also adds two further new configurables in the ["Checkpoints" group](https://www.postgresql.org/docs/9.6/static/runtime-config-wal.html#RUNTIME-CONFIG-WAL-CHECKPOINTS), `min_wal_size` and
  `checkpoint_flush_after`.

  As part of the upgrade procdure, running `chef-server-ctl cleanup` will remove Postgres 9.2's data
  and logs.

* Elasticsearch 5 support: Chef Server now supports Elasticsearch 5.
  This allows Chef Server and Chef Automate 1.6 to use the same Elasticsearch instance.

* [EPMD patch](https://github.com/chef/chef-server/pull/1328): The Erlang Port Mapper Daemon (EPMD)
  included in this package is now patched to only listen on the addresses specified in
  `ERL_EPMD_ADDRESS`. Before, it would implicitly add ::1 and 127.0.0.1 to the set of addresses to
  listen on, causing trouble for systems without a ::1.

  The preflight check that was in place to catch these situations has been removed.

* [RabbitMQ health check in status endpoint](https://github.com/chef/chef-server/pull/1345): Chef Server's
  `_status` endpoint now checks the health of the analytics and internal RabbitMQ vhosts. For these checks
  to work, the RabbitMQ management plugin must be installed. If it is not, the checks are not done. If
  Chef Server is configured not to use Actions, a check will not be performed against the Actions vhost.
  If an indexing queue is not used, the `chef_index` RabbitMQ vhost will not be checked.

* [Notification of affected services when updating secrets with set-secret](https://github.com/chef/chef-server/pull/1313):
  `chef-server-ctl set-secret` will notify the user of services that depend on
  the secret being changed. With the optional flag `--with-restart`,
  `chef-server-ctl set-secret` will attempt to automatically restart the
  dependent services.

## 12.15.8 (2017-06-20)

* [Stricter validation of non-functional user record fields](https://github.com/chef/chef-server/pull/1294),
  Chef Server now uses a regular expression to validate first, middle, and last name of a user
  on creation. The regex used is `[[:word:][:digit:]!'. -]*` (UTF-8). This tries to accommodate
  a wide range of names, while also strengthening Chef Server's role in preventing XSS attacks
  in web-based API clients. For compatibility reasons, a user's first, middle, or last name may
  also be `""` (empty string).
* [Search user by email case-insensitively](https://github.com/chef/chef-server/pull/1283):
  while technically only the host-part of an email address is to be treated case-insensitively,
  most email providers treat the _entire_ email address as case-insensitive. Chef Server now
  adopts that behaviour for _searching users_: querying for `user@host` (`GET /users?email=user%40host`)
  will now also return users with the recorded email of `USER@HOST` etc.
* API requests including an _unknown group_ now return 404 instead of 500 ([#1286](https://github.com/chef/chef-server/pull/1286))
* `opscode-erchef` now allows for configuring an optional ulimit via `opscode_erchef['memory_maxbytes']` ([#1279](https://github.com/chef/chef-server/pull/1279)).

* Fixed [regression](https://github.com/chef/chef-server/pull/1305) where credentials consumed
  by Analytics would be left plainly on disk after the `insecure_addon_compat` option was set to `false`.
* Fixed [regression](https://github.com/chef/chef-server/issues/1281) where parts of the available data
  (e.g. cookbook upload events) weren't sent to Chef Automate with the proper data collector token.

### Security Updates

* [Upgrade zlib to 1.2.11](https://github.com/chef/chef-server/pull/1311): this addresses [CVE-2016-9841](https://nvd.nist.gov/vuln/detail/CVE-2016-9841), [CVE-2016-9842](https://nvd.nist.gov/vuln/detail/CVE-2016-9842), and [CVE-2016-9843](https://nvd.nist.gov/vuln/detail/CVE-2016-9843).

## 12.15.7 (2017-05-16)

* Fixed [regression](https://github.com/chef/chef-server/issues/1274) that prevented
  some Open Source Chef 11 upgrades due to too-strict validations on object names in ACLs. This
  issue would also prevent editing of ACLs directly on node objects if the
  node name contained a ".".  This issue was introduced in 12.15.0
* Fixed [regression](https://github.com/chef/chef-server/pull/1272) that prevented the `reindex`
  command from working. This issue was a side effect of consolidating the multiple Erlang runtimes
  that we distributed with Chef Server into one for 12.15.0.

## 12.15.6 (2017-05-05)

* Fixed [regression](https://github.com/chef/chef-server/pull/1257) in oc-id.
  The identity service was using the wrong Chef Server API version level.
  .
## 12.15.5 (2017-05-04)

* Fixed [regression](https://github.com/chef/chef-server/pull/1253) in the nginx proxy
  that prevented Automate-based Compliance profiles from being reachable

## 12.15.3 (2017-05-03)

* Fixed [regression](https://github.com/chef/chef-server/pull/1246) in Bookshelf's preflight checks.
* Fixed regression that would cause Manage to be misconfigured
  to enable LDAP by default.
* PUT to `/users/USERNAME/_acl/PERM` will no longer return a 400 when the
  request is valid.

Note: There was no released version 12.15.1 or 12.15.2 due
      to issues identified internally.

## 12.15.0 (2017-04-27)

### Add required\_recipe endpoint

See Chef RFC 89 for a fuller description

Add the ability to serve a required recipe file to chef-clients.

`/organizations/<orgname>/required_recipe` returns 404 for all organizations
by default.

`/organizations/<orgname>/required_recipe` returns 401 when the request is
not made by a client from the requested org and the feature is
enabled.

`/organizations/<orgname>/required_recipe` returns the required recipe and
200 when the endpoint is enabled and requested by an authorized client.

`required_recipe["enable"]` in chef-server.rb enables the required recipe
feature.

`required_recipe["path"]` in chef-server.rb specifies the recipe file to
serve.

### ACLs and groups can refer to global groups

The server-admins group is useful, but it breaks roundtripping when it
appears in an organizations ACLs and groups. This was particularly
painful when using the API for backups.

We add a new syntax for referring to global objects from org local
context. ORGNAME::name and for global objects ::name. This can, and is
omitted whereever the context is clear. So if the server-admins
appears in an organizations ACL, you will see the name ::server-admins

### User customization of field mapping

Attributes from a user's LDAP record are used during account-linking to
populate the erchef user record when it is created. Previously, the
mapping between LDAP attributes and chef user attributes were fixed.
Now, they are configurable.

For example, if the user's LDAP record stores their email address in a
field named 'address' instead of 'mail', then you could set the
following in private-chef.rb:

ldap['email_attribute'] = "address"


## 12.14.0 (2017-03-23)

### Reduce password proliferation

We've substantially reduced the number of configuration files
that contain plaintext passwords. Now, no passwords or
credentials are rendered outside of `/etc/opscode/` in Chef
Server's default configuration.

To ensure backwards compatibility, Chef Server still renders
passwords and keys to multiple files in `/etc/opscode`. However,
if you are not using any Chef Server add-ons or if you have
updated to the latest releases of all add-ons, you can set:

   insecure_addon_compat false

in chef-server.rb and remove these other occurrences of secrets as
well.

If you are using LDAP integration, external postgresql, or other
Chef Server features that require providing passwords in
`/etc/opscode/chef-server.rb`, we've also provided commands that
allow you to set these passwords outside of the configuration
file. For information about these commands see:

https://docs.chef.io/ctl_chef_server.html#secrets-management

Note: Users of the DRBD-based HA configuration may still see
passwords related to keepalived and DRBD in /var/opt/opscode.

For further information see:

See [Chef Server Secrets Management](https://docs.chef.io/server_security.html#chef-server-credentials-management)
for more details.

## 12.13.0 (2017-02-20)

### New platform: RHEL6/s390x

Support for a new platform was added: Red Hat Enterprise Linux 6 on s390x.

### Solr4 Admin API/UI disabled by default

With this release, the admin UI of Solr4 has been removed. The underlying API
has also been disabled. Users that depend on the admin API endpoints can enable
them via adding

    opscode_solr4['enable_full_admin_api'] = true

to `chef-server.rb`.

### FIPS runtime flag exposed

The Chef Server package now exposes a `fips` configuration flag in
`chef-server.rb`. Setting `fips true` and reconfiguring will start the
server in FIPS mode. The default value of this flag is `false` except
on systems where FIPS is enabled at the Kernel where it defaults to `true`.

The only supported systems at this time for FIPS mode are RHEL. Packages for
other systems will be missing the required OpenSSL FIPS module and will fail
to start if reconfigured with `fips true`.

## 12.12.0 (2017-01-26)

This release addresses a number of bugs, the most notable are describe
below.

### `chef-server-ctl backup` correctly backs up configuration

Starting in version 12.10.0, a bug in the backup command produced
backups that did not include the configuration data in the resulting
tarball. This bug is now resolved. We recommend taking a new backup
after upgrading to 12.12.0.

### Search respects rows parameter when using ElasticSearch

When configured to use ElasticSearch, the Chef Server now correctly
respects the `rows` parameter to search requests rather than returning
all rows.

We recommend upgrading to 12.12.0 for all users of Chef Backend to
receive this search fix.

### Solr 4 GC Logging

Chef Server now uses Java's native rotation for the gclog.  This
prevents situations where logrotate creates large sparse files on disk
which may be problematic to manage with tools that can't handle sparse
files.

As a consequence of this change, the Solr 4 GC log can now be found at
`/var/log/opscode/opscode-solr4/gclog.log.N.current` where N is an
integer. The `.current` extension denotes the log currently being
written to.

To remove the older GC logs, run `chef-server-ctl cleanup` after
upgrading.

To suppress the GC Log completely, the Chef Server now accepts the
following option in `/etc/opscode/chef-server.rb`:

    # true (default) to enable gc logging,
    # false to disable gc logging
    opscode_solr4['log_gc'] = false

### oc_id email configuration options

The oc_id service now includes configuration for outbound email to
ensure password reset emails can be sent correctly.  Chef Server now
accepts the following options in `/etc/opscode/chef-server.rb`:

    # defaults to the value of the from_email configuration option
    oc_id['email_from_address'] = "oc_id@example.com"
    # defaults to the api_fqdn
    oc_id['origin'] = "mail.yourco.io"

## 12.11.1 (2016-11-18)

This release addresses an incompatibility with Push Jobs Server 1 by:

- Re-enabling TLS 1.0 and 1.1 in the default TLS configuration
- Re-enabling the AES cipher suite in the default TLS configuration

Further, Chef Server is now available on the s390x platform.

## 12.11.0 (2016-11-10)

### Chef Server

- A new endpoint

        /organizations/ORGNAME/validate/PATH

  is now available. This endpoint accepts a signed request and
  validates it as if it had been sent to `PATH`. It returns 200 if the
  request is authentic and 401 if it is not.

- A new endpoint

        /organizations/ORGNAME/data-collector

  is now available. This endpoint forwards requests for a
  data-collector service after authenticating the request using Chef
  Server's standard authentication headers.  To use this endpoint,
  users must set both of the following options in
  `/etc/opscode/chef-server.rb`:

        data_collector['token']
        data_collector['root_url']

- A new endpoint

        /organizations/ORGNAME/owners/OWNER/compliance[/PROFILE]

  is now available. This endpoint forwards requests for compliance
  profiles to a user-configurable Chef Automate server after
  authenticating the request using Chef Server's standard
  authentication headers. To use this endpoint, users must set both
  of the following options in `/etc/opscode/chef-server.rb`:

        profiles['root_url']
        data_collector['token']

### Security Updates

- The default allowed SSL ciphers now include AES256-GCM-SHA384 to
  ensure compatibility with AWS's Classic ELB health check tool.

- `chef-server-ctl psql` previously revealed the postgresql password
  via `ps`.

## 12.10.0 (2016-10-31)

### Chef Server
- Smaller download - the download size has been reduced by around 35% via removal of redundant, cached, and unused components. The installed size has been similarly reduced.
- add retry support to opscode-expander
- `chef-server-ctl reindex` will now continue even if some objects are
  not indexable, and will show which objects failed at the conclusion
  of the run.
- Data Collector support for Policyfiles.
- `chef-server-ctl install` add-on installation now pulls from the
  correct source.
- Regression fix: that caused errors on reconfigure when LDAP bind password
  is nil has been fixed.

### Security Updates

* Upgrade to OpenSSL 1.0.2j.  The prior release (1.0.1u) is approaching EOL.
* Updated TLS ciphers. See compatibility notes, below.

### Compatibility Notes

1. The change of TLS ciphers can cause older tooling to fail to negotiate
   SSL sessions with the Chef Server. The changes to the cipher list are
   captured [here](https://github.com/chef/chef-server/pull/918#issuecomment-244430458).
   Upgrading any custom clients of the Chef Server API to use a current SSL
   release will resolve this.
   * Alternatively, you can set `nginx['ssl_protocols']` in
     `/etc/opscode/chef-server.rb` to a set of ciphers that are
     compatible with your tooling, then running `chef-server-ctl
     reconfigure` to pick up the changes.
2. With this TLS cipher suite change, the Reporting add-on will report
   errors when `opscode-reporting-ctl test` is run.  A fix for this is
   available in the `current` channel for reporting, and will be
   released to stable in November. This issue does not otherwise affect the
   Reporting add-on, but you can resolve this locally by modifying
   `/etc/opscode-reporting/pedant_config.rb` and adding the following
   line:
```
ssl_version :TLSv1_2
```

## 12.9.1 (2016-9-26)

### Security Updates

* The update of OpenSSL 1.0.1u addresses the following:
  * [CVE-2016-6304](https://www.openssl.org/news/vulnerabilities.html#2016-6304)
  * [CVE-2016-2183](https://www.openssl.org/news/vulnerabilities.html#2016-2183)
  * [CVE-2016-6303](https://www.openssl.org/news/vulnerabilities.html#2016-6303)
  * [CVE-2016-6302](https://www.openssl.org/news/vulnerabilities.html#2016-6302)
  * [CVE-2016-2182](https://www.openssl.org/news/vulnerabilities.html#2016-2182)
  * [CVE-2016-2180](https://www.openssl.org/news/vulnerabilities.html#2016-2180)
  * [CVE-2016-2177](https://www.openssl.org/news/vulnerabilities.html#2016-2177)
  * [CVE-2016-2178](https://www.openssl.org/news/vulnerabilities.html#2016-2178)
  * [CVE-2016-2179](https://www.openssl.org/news/vulnerabilities.html#2016-2179)
  * [CVE-2016-2181](https://www.openssl.org/news/vulnerabilities.html#2016-2181)
  * [CVE-2016-6306](https://www.openssl.org/news/vulnerabilities.html#2016-6306)

##### Compatibility Notes

* This release still has a known issue in the user ACL endpoint:

  > ACLs: updating ACLs of a specific user (`/users/USER/_acl`) will not
  > succeed.  This undocumented API is very rarely used and is not supported by
  > tooling provided by Chef Software.
  >
  > If you make internal use of PUTs to this endpoint, please wait to upgrade.

## 12.9.0 (2016-09-22)

### Chef Server

* clean up legacy expander-reindexer service
* Fix logrotate configuration to make it work with SELinux enabled
* constrain ACL updates so that all users provided in an ACE must also
  be in the organization. Users from outside the organization that are
  already in the ACL of an object or container will not be removed.
  *  Note that we do not recommend adding users
    directly to ACLs - instead, the best approach is to use groups and
    control membership based on your organizational policies
  * See important compatibility note below.
* ACL updates now permit adding a client to ACLs when a user of the same
  name exists in the system [111](https://github.com/chef/chef-server/issues/111)
* chef-server-ctl user-delete will now report in which organizations the
  user is an adminstrator of when that blocks deletion.  It also
  provides a new option to attempt to auto-remove those users from the
  admin groups, but will prevent removal if doing so would leave the
  admin group(s) empty.
* The Identity Management component will now send a verification email
  when updating email addesses, and the update will be made only after
* Identity Management now uses secure cookies.
  the verification link is clicked.
* LDAP bind passwords now support special characters.
* Fix crash that can occur when logging into an ldap-enabled server when
  bypassing LDAP.
* multiple improvements to DVM, the development environment that resides in
  the Chef Server repository

#### Security

* This release includes a fix for an issue where policies of the
  same name could be accessed across organizations [643](https://github.com/chef/chef-server/pull/643)
* Fixed logging LDAP password in event of some errors [156](https://github.com/chef/chef-server/issues/156)

#### API Changes

* The object ACL update API (`/organizations/ORG/OBJECT_TYPE/OBJECT_NAME/_acl/PERM`)
  now requires that any users being added to an object's ACL exist in the same
  organization as the object itself.  Existing users that are not organization members
  and have already been added to an ACL will not be affected, and can still be seen in the GET
  response for this API.
* When a client and a user of the same name exist in the organization,
  the HTTP response code `422` is returned and the error text will
  indicate which client/user was ambiguous.
* The object ACL API now accepts `clients` and `users`
  fields which will allow you to disambiguate in this situation by separating
  clients from users in the request. When using this option, the `actors` field
  must be set to `[]` in the request.
* 400 responses in object ACL updates will now include the specific
  missing or incorrect entities where appropriate.
* The GET API response for object ACL has not been changed by default,
  but if the query argument `?detail=granular` is provided, the json
  body will contain `actors` which will be an empty array; and
  `clients:[...]`, `users:[...]`.  This body is compatible
  with the PUT API changes, and can be used in that request without
  modification.

##### Compatibility Notes

* ACLs: updating ACLs of a specific user (`/users/USER/_acl`) will not
  succeed.  This undocumented API is very rarely used and is not supported by
  tooling provided by Chef Software.

  If you make internal use of PUTs to this endpoint, please wait to
  upgrade.

  This change is being tracked as [#938](https://github.com/chef/chef-server/issues/938).
* ACLs: users must be a member of an organization in order to be added
  to the ACLs of an object within an organization. If you GET an ACE
  that contains a user not in the org, you will not be able to re-PUT
  the same ACE.

## 12.8.0 (2016-07-06)

### Chef Server

* Initial support for sending updates to a
  [Chef RFC 077](https://github.com/chef/chef-rfc/blob/master/rfc077-mode-agnostic-data-collection.md)-compliant
  datacollection service.

* Minor bug fixes in the postgresql preflight check and upgrade
  migration 29.
