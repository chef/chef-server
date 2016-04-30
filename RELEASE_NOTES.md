# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.6.0

### Chef Server
  * Fixed false failures for postgres preflight check.
  * Fixed bug where public\_key\_read\_access group was not properly being respected for access on org-scoped user and client public key reads.
  * Update licensing handling to make licensing information easier to find (see [here](https://www.chef.io/blog/2016/04/26/changes-to-how-chef-products-handle-licenses/)).
