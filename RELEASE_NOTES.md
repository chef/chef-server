# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current major release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.8.1

### Chef Server
* The object ACL API now requires that any users being added to an
  object's ACL exist in the same organization as the object itself.
  Existing users that are not organization members and have already been added
  to an ACL will not be affected.



## 12.8.0 (2016-07-06)

### Chef Server

* Initial support for sending updates to a
  [Chef RFC 077](https://github.com/chef/chef-rfc/blob/master/rfc077-mode-agnostic-data-collection.md)-compliant
  datacollection service.

* Minor bug fixes in the postgresql preflight check and upgrade
  migration 29.
