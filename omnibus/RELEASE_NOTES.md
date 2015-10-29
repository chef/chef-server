# Chef Server Release Notes

## 12.2.0 (2015-09-01)

The following items are new since Chef Server 12.1.2 and/or are changes from
previous versions.  For specific breakdown of updated components, refer to
CHANGELOG.md.

### Chef Server

  * New Policyfile endpoints have been added to enable cleanup of policy
    objects.  See API Changes below for more detail.
  * Org Policy Changes:
    * rename `ORG_global_admins` to `ORG_read_access_group`
    * new restrictions that prevent removing `admins` group ACE from a
      group's ACL.
    * users in the same org can now by default read each others' user
      records.
    * See "Organization Policy Changes" below for more details.
  * It is now possible to configure a new installation of Chef Server to
    use a self-hosted PostgreSQL instance, including Amazon RDS
    PostgreSQL. See [this page](https://docs.chef.io/release/server_12-2/release_notes.html#external-postgresql) for details.
    * Note that direct upgrades to an external postgresql instance are
      not supported at this time.  Instead,  back up the existing Chef
      Server using knife ec backup, bring up a new Chef Server with
      external PostgreSQL configure, then use knife ec restore.
    * If you wish to use `chef-server-ctl cleanse` and have it delete
      data from an external PostgreSQL instance as well, you must use
      the new flag `--with-external`.

#### chef-server-ctl

  * New commands `backup` and `restore` which you can use to back up
    to/restore a Chef Server's data in its entirety.
      * To use this feature at this time, you will need to ensure that
        `rsync` is installed on the server ahead of time.
      * If you have enabled external postgresql, use `knife ec backup'
        instead of chef-server-ctl backup/restore backup/restore, as
        chef-server-ctl backup/restore are not yet compatible with
        external data stores.
  * New command `chef-server-ctl psql <service-name>` to log in via `psql` with
    read-only access to the the postgresql database used by the named
    service.  This is now the preferred way to log into a Chef Server
    database, and will automatically handle authentication. Use
    `--write` to enable write access.
  * `reindex` now supports server-wide reindexing.
  * Improved error output.  When errors exist in `chef-server.rb`
    or we detect errors in external postgresql setup, we now provide clear
    errors and - where appropriate - links to supporting documentation.
    This is an ongoing process, so further improvements should be expected
    in this space.
  * Enhancements to the chef-server-ctl add-on framework now permit custom
    topologies.
  * `status`, `cleanse`, `service-list` commands have been updated to
    support external PostgreSQL.  Other service commands have been
    disabled when external PostgreSQL is in use.
  * chef-server-ctl will now refuse to run as anyone other than root,
    instead of giving a confusing stack trace when this is attempted.

#### Chef Server Development Environment Improvements

  * Chef Server's self-contained development environment 'dvm' has been
    updated to support loading of omnibus gems, and with support for
    auto-loading omnibus components before the first VM reconfigure has
    been run.
  * For testing/experimenting with external PostgreSQL,
    `dev/config.yml.example` shows how to enable a second `database` VM
    in dvm. This will hosting an external PostgreSQL database, and
    configures Chef Server to enable on the primary VM.

### Authorization Service (oc\_bifrost)
 * A deadlock has been resolved that can occur when concurrent
   updates to the same actor are made.  Consistent with the overall
   Chef Server API, the last update in will be the one that takes
   precedence.

### Identity Service (oc\_id)
 * Additional steps have been taken to ensure that unless newrelic support is expressly
   configured, the service will not call home to newrelic. This fixes a
   problem that affects 12.1.0, 12.1.1, 12.1.2.
 * Ensure that `oc-id` is reachable from the load balancer under ipv4 and ipv6.

### Deprecations

There are no deprecations to announce in this release.

### Bug Fixes / Closed Issues
* [CHEF-SERVER-154](https://github.com/chef/chef-server/issues/154)
  Do not enable automatic/unattended upgrades of Chef Server on RHEL when
  the packagecloud repository has been added for add-ons are present.
  If this has already been enabled prior to this point, disable it on
  the next reconfigure.
* [CHEF-SERVER-437](https://github.com/chef/chef-server/issues/437)
  The license endpoint now only reports on nodes associated with an
  organization.
* When there is a username/client collision and the request originates
  from Manage, ensure that user is used to authenticate the request
  instead of client.
* When upgrading from EC11 and OSC11, custom solr configuration
  is preserved.
* Multi-byte encoding issues in dependency resolution (depsolver) have
  been fixed.
* Newrelic support is now disabled for all profiles in oc-id unless
  explicitly configured.
* `--expiration-date` is no longer ignored by the `add-*-key`
  commands.

### API Changes and Additions
 * New Policyfile endpoints, available under v0 and later:
   * `/policies/:policy_name` (GET, DELETE)
   * `/policies/:policy_name/revisions` (POST)
   * `/policies/:policy_name/revisions/:revision_id` (GET, DELETE)
   * `/policy_groups/:policy_group_name` (GET, DELETE)
 * Attempts by anybody other than superuser to PUT a group ACL that
   does not contain a grant ACE for the `admins` group will receive a
   403.

### Security Updates
 * upgraded to openssl-1.0.1p for CVE-2015-1793.   Note that Chef Server
   was not directly affected by this CVE.

### Organization Policy Changes
* It is no longer possible to remove an organization's admin group's grant ACE
  from a group. This prevents a class of errors where the admins group is
  inadvertantly removed, preventing subsequent updates to the group's ACL.
* The group misleadingly named `$ORG_global_admins` where $ORG is the
  organization's name has been renamed to `$ORG_global_read_group`, which more
  accurately affects the access given to that group.
* Users that share an organization will be able to read each other's user records.
  This allows users to encrypt data bag items for each other using `knife-vault`
  without granting these users admin access.
  * Being able to read another user's user object means you can look up that
    person's public keys registered with the chef server, shared organizations,
    email address, and name.
  * Users can *only* see the org memberships when those organizations
    are in common to both users.  They can *not* see organziation
    memberships that are not shared.



