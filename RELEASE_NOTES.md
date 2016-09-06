# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current major release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.8.1

### Chef Server

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

## 12.8.0 (2016-07-06)

### Chef Server

* Initial support for sending updates to a
  [Chef RFC 077](https://github.com/chef/chef-rfc/blob/master/rfc077-mode-agnostic-data-collection.md)-compliant
  datacollection service.

* Minor bug fixes in the postgresql preflight check and upgrade
  migration 29.
