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

- Chef Software Inc. Slack account
- VPN account for Chef Software Inc.
- Account on [https://discourse.chef.io](https://discourse.chef.io) using your Chef email address
- Access to automate.chef.co

:warning: If it is a Friday -- do we really need that release today? :warning:

## THE PROCESS

### Informing everyone of a pending release.

- [ ] Announce your intention to drive the release to #cft-announce and #chef-server on slack.

### Testing the Release

> This process is currently manual. There is working being done to fold this into the ACC pipeline.

Every commit to chef-server master is tested against a full pedant
run. However, upgrade testing must still be done in advance of the
release:

- [ ] Run the Chef Automate-based automated upgrade testing.
  * Clone the `pool/qa-chef-server-cluster` project from automate.chef.co.
    * If you get an access rights error, make sure your public github key is included in your delivery profile.
  * On a branch, update relevant attributes in `.delivery/build/attributes/default.rb`:
    * Update `chef_server_test_url_override` to be the URL corresponding to
      the latest package you'd like to test at artifactory.chef.co (probably
      a git poll build that you want to ship as the next version).
      Remember to use the current channel instead of the unstable stable on
      artifactory as those are the builds that passed tests on wilson.
    * Ensure that all entries in `default['delivery-matrix']['acceptance']['matrix']`
      array are uncommented (except for `ha_ec_upgrade_aws`).
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
    release, ask #acc-support, #chef-server, or #automate-support for help.
    * If automate.chef.co builders are too constrainted to run the full matrix
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

If one of these tests has failed, you cannot ship a release until it's fixed.
Note, no changes other than CHANGELOG/RELEASE_NOTES changes should
land on master between testing and releasing since we typically tag
HEAD of master. If something large does land on master, the release
tag you create should point specifically at the build that you tested.
The git SHA of the build you are testing can be found in
`/opt/opscode/version-manifest.json`.

### Preparing for the release

- [ ] Check RELEASE_NOTES.md to ensure that it describes the
  most important user-facing changes in the release. This file should
  form the basis of the post to Discourse that comes in a later step.
- [ ] Open a PR with these changes (applying the `Omnibus: Skip Build` label)

### Building and Releasing the Release

- [ ] Select a version from the `current` channel that you wish to promote to `stable`.
  Make sure that this version has gone through the upgrade testing. 
- [ ] Use expeditor to promote the build:

        /expeditor promote chef-server VERSION

  Please do this in the `#releng-support` room.  Once this is
  done, the release is available to the public via the APT and YUM
  repositories and downloads.chef.io.

- [ ] Chef employees should already know a release is coming; however, as a
  courtesy, drop a message in the #cft-announce slack channel that the release
  is coming. Provide the release number and any highlights of the release.

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

Chef Server is now released.
