# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current major release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

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

3. Users of Push Jobs Server 1.1.6 or older should re-enable TLS 1.1
   by putting the following in `/etc/opscode/chef-server.rb`

        nginx['ssl_protocols'] = "TLSv1.1 TLSv1.2"

   and running `chef-server-ctl reconfigure`.


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

* The fixes to the ACL apis which 12.9.0 referenced being fixed in 12.9.1 have been
  delayed to 12.9.2 in order to release this security update:

  > ACLs: updating ACLs of a specific user (`/users/USER/_acl`) will not
  > succeed.  This undocumented API is very rarely used and is not supported by
  > tooling provided by Chef Software.

  > If you make internal use of PUTs to this endpoint, please wait until 12.9.2
  > to upgrade.  This will fix both the newly introduced issue, as well as an
  > older issue that prevented the endpoint from working in many other cases in
  > 12.8 and prior.

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

  If you make internal use of PUTs to this endpoint, please wait until 12.9.2
  to upgrade.  This will fix both the newly introduced issue, as well as an
  older issue that prevented the endpoint from working in many other cases in
  12.8 and prior.

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
