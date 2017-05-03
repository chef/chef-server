# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current major release and all patches.
For prior releases, see
[PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.15.2 (2017-05-03)

* Fixed regression [regression](https://github.com/chef/chef-server/pull/1246) in Bookshelf's preflight checks.
* Fixed regression that would cause Manage to be misconfigured
  to enable LDAP by default.
* PUT to `/users/USERNAME/_acl/PERM` will no longer return a 400 when the
  request is valid.

## 12.15.0 (2017-04-27)

### Add required_recipe endpoint

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
