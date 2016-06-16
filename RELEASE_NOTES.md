# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.7.0

### Chef Server
  * Add support for service credential rotation via [Veil](https://github.com/chef/chef-server/blob/3ff412b5a2e6ad54cfa79bca6865e1bbca28fe5e/omnibus/files/veil/README.md), and for requiring it via the added `require-credential-rotation` command of `chef-server-ctl`.
  * Fixed existing process detection in oc_id which caused high CPU utilization
  * Fixed bug where deleting a user would not delete pending invites.
  * Allow filtering users by their external_authentication_uid, allowing for SAML-authentication in Chef-Manage.

### Security Updates

* The update of OpenSSL 1.0.1t addresses the following:
  * [CVE-2016-2105](https://www.openssl.org/news/vulnerabilities.html#2016-2105)
  * [CVE-2016-2106](https://www.openssl.org/news/vulnerabilities.html#2016-2106)
  * [CVE-2016-2107](https://www.openssl.org/news/vulnerabilities.html#2016-2107)
  * [CVE-2016-2109](https://www.openssl.org/news/vulnerabilities.html#2016-2109)
  * [CVE-2016-2176](https://www.openssl.org/news/vulnerabilities.html#2016-2176)

### API Changes

  * `GET /users` now allows filtering by `external_authentication_uid`.
    For example, `GET /users?external_authentication_uid=jane%40doe.com` will
    now return the users whose external_authentication_uid is `jane@doe.com`.
    This change is available under API v0 and later.
