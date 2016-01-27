# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.4.0 (Unreleased)

### `oc-id`
    * Pulled in a new version of Rails for `oc-id` due to critical vulnerabilities found in Rails: http://weblog.rubyonrails.org/2016/1/25/Rails-5-0-0-beta1-1-4-2-5-1-4-1-14-1-3-2-22-1-and-rails-html-sanitizer-1-0-3-have-been-released/

### Chef Server
  * There is now a Universe endpoint, which provides the same output
    as the Supermarket or berkshelf-api universe endpoints.
  * Implemented Server Admins. Server Admins is a list of users that that full permissions access on the users endpoint. You can add and remove users from the Server Admins list. This allows non-superusers to perform creation, deletion, editing, and listing on all users (except the superuser) in a Chef Server, granting great flexibility around user management.
  * Implemented Chef Authentication Signing Protocol v1.3 [rfc065](https://github.com/chef/chef-rfc/blob/master/rfc065-sign-v1.3.md)

### API Changes and Additions
  * `/universe` (GET)
