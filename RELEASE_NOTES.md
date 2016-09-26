# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current major release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

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
