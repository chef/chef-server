# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.5.0

### Chef Server
  * Added /orgs/org/users/user/keys(/key) endpoint and changed default perms on org scoped key GETs.
		The following endpoints' GET methods can now be accessed by any requestor that is a member of the same organization:
		/organizations/:org/clients/:client/keys
		/organizations/:org/clients/:client/keys/:key
		/organizations/:org/users/:user/keys
		/organizations/:org/users/:user/keys/:key
		The above org-scoped user keys endpoints are new and access to them can be controlled by an admin by editing memebership
		of the public_key_read_access group.
	* Added support for Ubuntu 14 Trusty.
	* Bugfixes for chef-server-ctl backup.

### JRE
  * Updated to 8u74.

### OpenSSL
  * Updated to 1.0.1s to mitigate DROWN.

### NodeJS
  * Updated to 0.10.35 to mitigate CVE-2013-4450.

## Ruby On Rails
  * Updated rails to 4.2.5.2 to mitigate CVE-2016-2097 and CVE-2016-2098.

## PostgreSQL
  * Updated to 9.2.15.
