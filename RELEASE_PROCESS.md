# Chef Server Release Process

## Document Purpose

The purpose of this document is to describe the *current* release
process such that any member of the team can do a release.  As we
improve the automation around the release process, the document should
be updated such that it always has the exact steps required to release
Chef Server.

This document is NOT aspirational.  We have a number of automation
tools that we intend to use to improve the release release process;
however, many are not fully integrated with our process yet. Do not
add them to this document until they are ready to be part of the
release process and can be used by any team member to perform a
release.

## Pre-requisites

In order to release, you will need the following accounts/permissions:

- Local checkouts of the chef-server and chef-web-downloads repositories
- Push access to the chef-server github repository
- Chef Software, Inc Slack account
- VPN account for Chef Software, Inc.
- Account on [https://discourse.chef.io](https://discourse.chef.io) using your Chef email address
- Login for wilson.ci.chef.co (This is linked to your github account.)
- Access to artifactory.chef.co
- Access to delivery.chef.co
- The CHANGELOG_GITHUB_TOKEN environment variable set to a github token gathered
  [here](https://github.com/settings/tokens/new?description=GitHub%20Changelog%20Generator%20token).
  Read access to repositories and issues should be sufficient. (Check the "Full control of private repositories" box.)
- Run `export CHANGELOG_GITHUB_TOKEN="«your-40-digit-github-token»"` to set your new token that you just created
- Install Github Changelog Generator
  + `gem install github_changelog_generator`


## THE PROCESS
### Informing everyone of a pending release.

- [ ] Announce your intention to drive the release to #cft-announce on slack.
- [ ] Cross-post this intention on #chef-server and #spool.  Determine whether
  this upgrade requires a major/minor/patch bump and what the major changes
  are.
- [ ] Ensure that the chef-server pipeline on wilson is currently green - you
  cannot ship if master is broken.

### Testing the Release

Every commit to chef-server master is tested against a full pedant
run. However, upgrade testing must still be done in advance of the
release:

- [ ] Run the Chef Delivery-based automated upgrade testing.
  * Clone the `engineering-services/qa-chef-server-cluster` project from delivery.chef.co.
    * If you get an access rights error, make sure your public github key is included in your delivery profile.
  * On a branch, update relevant attributes in `.delivery/build/attributes/default.rb`:
    * Update `chef_server_test_url_override` to be the URL corresponding to
      the latest package you'd like to test at artifactory.chef.co (probably
      a git poll build that you want to ship as the next version).
      Remember to use the current channel instead of the unstable stable on
      artifactory as those are the builds that passed tests on wilson.
    * Ensure that all entries in `default['delivery-matrix']['acceptance']['matrix']`
      array are uncommented.
  * Commit the change and run `delivery review`.
  * When the lint and unit tests pass, approve the change and watch the
    matrix of tests run. This change will spin off several new changes, so look for
    them in the root of the project in the delivery UI.
    After your change, you should see one change per entry in the upgrade test matrix.
    It will be named "Merged change...".
  * If you had no failures, AWS instances will be cleaned up for you and the cookbook cleans up any
    instances over 24 hours old the next time it runs, but it's good to log into
    AWS and look for any machine instances tagged with
    `qa-chef-server-cluster-delivery-builder` in `US West (Oregon) us-west-2`
    region and delete them once all your tests have finished to make sure we
    don't leave any unused instances
    up.
  * If you did have test failure due to an environmental issue unrelated to the
    release, ask #chef-server and #delivery-support for help.
    * If delivery.chef.co builders are too constrainted to run the full matrix
      of 10 jobs comment the second half of the entries in the
      `default['delivery-matrix']['acceptance']['matrix']` array and run a build.
      After that build and all its matrix builds finish, run another build with the first half
      commented and the second half uncommented (see below for where to find matrix builds).
      Regardless, you should make sure every matrix entry gets run before you ship,
      so always double-check these attributes, even if running the full matrix, as some
      might be commented out from previous runs.
- [ ] If this release is being made to address a specific
  high-urgency, high-severity customer issue or security issue, please
  *specifically* test that the issue in question is fixed.

If one of these tests has failed, you cannot ship a release today.
Note, no changes other than CHANGELOG/RELEASE_NOTES changes should
land on master between testing and releasing since we typically tag
HEAD of master. If something large does land on master, the release
tag you create should point specifically at the build that you tested.
The git SHA of the build you are testing can be found in
`/opt/opscode/version-manifest.json`.

### Preparing for the release

- [ ] Check that omnibus/config/projects/chef-server.rb has the
  correct version given the type of changes in this release.

- [ ] Download the previous release of Chef Server (.deb).

- [ ] Run `./dev/scripts/update-changelog.sh /path/to/deb/from/previous/step`

- [ ] Grab the section on the latest version from `NEW_CHANGELOG.md` and paste
  it at the top of `CHANGELOG.md`.

- [ ] Copy the components section from `MODIFIED_COMPONENTS_CHANGELOG.md` and
  paste into `CHANGELOG.md`

- [ ] Check RELEASE_NOTES.md to ensure that it describes the
  most important user-facing changes in the release. This file should
  form the basis of the post to Discourse that comes in a later step. Update as
  appropriate.  If a there is a major version change, transfer all
  contents to PRIOR_RELEASE_NOTES.md before adding release notes.

### Building and Releasing the Release

- [ ] Tag the chef-server repository with the release version: `git
  tag -a VERSION_NUMBER`. The first line of the tag message should be
  the version number. The other lines are up to you. You're the boss!
  (You can leave them blank).

- [ ] Push the new tag: `git push origin master --tags`.

- [ ] Trigger a release build in Jenkins using the
  `chef-server-12-trigger-release` trigger.  Use the tag you created
  above as the GIT_REF parameter.

- [ ] Wait for the pipeline to complete.

- [ ] Use julia to promote the build: `@julia artifactory promote
  chef-server VERSION`.  Please do this in the
  `#eng-services-support` room.  Once this is done, the release is
  available to the public via the APT and YUM repositories.

- [ ] Chef employees should already know a release is coming; however, as a
  courtesy, drop a message in the #cft-announce slack channel that the release
  is coming. Provide the release number and any highlights of the release.

- [ ] In your local checkout of the chef-web-downloads repository,
generate an update to the download pages using rake:

```
git co -b YOUR_INITIALS/release-chef-server-VERSION
rake fetch[chef-server]
git add data/
# make sure all the changes are what you expect
# write a simple commit message
git commit -v
git push origin YOUR_INITIALS/release-chef-server-VERSION
```

- [ ] Open a GitHub pull request for the chef-web-downloads repository
based on the branch you just created.

- [ ] Have someone review and merge the PR.

- [ ] Once committed to master, an automated process will make the
  change to the live website; however, it might take a few minutes for
  the deploy to complete and then a few more for CDN caches to expire.

- [ ] Write and then publish a Discourse post on https://discourse.chef.io
  once the release is live. This post should contain a link to the downloads
  page ([https://downloads.chef.io](https://downloads.chef.io)) and its contents
  should be based on the information that was added to the RELEASE_NOTES.md file
  in an earlier step. *The post should  be published to the Chef Release
  Announcements category on https://discourse.chef.io. If it is a security
  release, it should also be published to the Chef Security Announcements
  category.* Full details on the policy of making release announcements on
  Discourse can be found on the wiki under the Engineering section ->
  Policy and Processes -> Release Announcements and Security Alerts

- [ ] Let `#cft-announce` know about the release, including a link to the Discourse post.

Chef Server is now released.

## Post Release

- [ ] Bump the Chef Server version number in
  `omnibus/config/projects/chef-server.rb` for development to the next
  logical version number.
